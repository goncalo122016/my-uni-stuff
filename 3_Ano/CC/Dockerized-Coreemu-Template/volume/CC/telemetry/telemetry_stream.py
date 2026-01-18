import asyncio
import socket
import logging
import time
import random
from typing import Optional, Dict
from common.message_types import TelemetryMessage
from common.protocol_config import ProtocolConfig
from .telemetry_manager import TelemetryManager

class TelemetryStreamServer:
    def __init__(self, telemetry_manager: TelemetryManager, mothership, host: str = '0.0.0.0', port: int = None):
        self.telemetry_manager = telemetry_manager
        self.mothership = mothership
        self.host = host
        self.port = port or ProtocolConfig.TELEMETRY_STREAM_PORT
        self.logger = logging.getLogger(__name__)
        self.server = None
        self.clients: Dict[str, asyncio.StreamWriter] = {}  # rover_id -> writer
        self._last_sensor_report: Dict[str, float] = {}  # rover_id -> timestamp
        self._rng = random.Random()

    async def start_server(self):
        """Inicia servidor de telemetria"""
        self.server = await asyncio.start_server(
            self.handle_client,
            self.host,
            self.port
        )
        
        addr = self.server.sockets[0].getsockname()
        self.logger.info(f"[TS] Servidor de telemetria iniciado em {addr}")
        
        async with self.server:
            await self.server.serve_forever()

    def _process_sensors(self, telemetry: TelemetryMessage):
        """
        Calcula o estado de cada sensor e devolve (status, detalhes),
        mas **NÃO** altera o estado do rover. Quem decide ir para ERROR
        é o próprio rover.
        """
        rover_id = telemetry.rover_id

        # 1) Valores base vindos da telemetria
        temp = getattr(telemetry, "temperature", None)
        bat = getattr(telemetry, "battery", None)

        # 2) Flags de sensores enviados pelo rover (bits 0–3)
        flags = getattr(telemetry, "sensor_flags", 0)

        temp_flag = bool(flags & (1 << 0))
        bat_flag = bool(flags & (1 << 1))
        tires_flag = bool(flags & (1 << 2))
        antenna_flag = bool(flags & (1 << 3))

        # 3) Estado dos sensores baseado nas flags
        temp_ok = not temp_flag
        bat_ok = not bat_flag
        tires_ok = not tires_flag
        antenna_ok = not antenna_flag

        # 4) Info extra sobre a bateria (só para logs)
        battery_low = False
        battery_out_of_range = False
        if bat is not None:
            if bat < 0 or bat > 100:
                battery_out_of_range = True
            elif bat <= 5:
                battery_low = True

        status = {
            "temperature": temp_ok,
            "battery": bat_ok,
            "tires": tires_ok,
            "antenna": antenna_ok,
        }

        detalhes = {
            "temp_value": temp,
            "battery_value": bat,
            "battery_low": battery_low,
            "battery_out_of_range": battery_out_of_range,
        }

        # 5) Apenas logs, não mexe no estado do rover
        if battery_out_of_range:
            self.logger.error(
                f"[TS] ERRO de leitura no sensor de bateria do rover {rover_id}: "
                f"valor fora de [0, 100]% (leitura: {bat:.1f}%)."
            )

        return status, detalhes

    def _maybe_report_sensors(self, telemetry: TelemetryMessage, status: Dict[str, bool], detalhes: Dict):
        """Mostra lista de sensores a funcionar no arranque e a cada 30s."""
        rover_id = telemetry.rover_id
        now = time.time()
        last = self._last_sensor_report.get(rover_id)

        if last is None or (now - last) >= 30.0:
            self._last_sensor_report[rover_id] = now
            self._log_sensor_status(telemetry, status, detalhes)

    def _log_sensor_status(self, telemetry: TelemetryMessage, status: Dict[str, bool], detalhes: Dict):
        """Escreve no log o estado dos sensores do rover."""
        rover_id = telemetry.rover_id

        temp = detalhes["temp_value"]
        bat = detalhes["battery_value"]
        battery_low = detalhes["battery_low"]
        battery_out_of_range = detalhes["battery_out_of_range"]

        temp_ok = status["temperature"]
        bat_ok = status["battery"]
        tires_ok = status["tires"]
        antenna_ok = status["antenna"]

        self.logger.info(f"[TS] Verificação de sensores do rover {rover_id}:")

        # Temperatura
        if temp is None:
            self.logger.info("[TS]   - Sensor temperatura: NÃO FUNCIONAL (sem leitura)")
        else:
            if temp_ok:
                self.logger.info(
                    f"[TS]   - Sensor temperatura: FUNCIONAL "
                    f"(leitura atual: {temp:.1f}ºC)"
                )
            else:
                self.logger.info(
                    f"[TS]   - Sensor temperatura: NÃO FUNCIONAL "
                    f"(leitura atual: {temp:.1f}ºC)"
                )

        # Ar dos pneus
        self.logger.info(
            f"[TS]   - Sensor ar dos pneus: "
            f"{'FUNCIONAL' if tires_ok else 'NÃO FUNCIONAL'}"
        )

        # Bateria
        if bat is None:
            self.logger.info(
                "[TS]   - Sensor nível de bateria: NÃO FUNCIONAL (sem leitura)"
            )
        else:
            if battery_out_of_range:
                self.logger.info(
                    f"[TS]   - Sensor nível de bateria: NÃO FUNCIONAL "
                    f"(ERRO: valor fora de 0-100%, leitura atual: {bat:.1f}%)"
                )
            elif battery_low:
                self.logger.info(
                    f"[TS]   - Sensor nível de bateria: NÃO FUNCIONAL "
                    f"(nível muito baixo: {bat:.1f}%)"
                )
            else:
                self.logger.info(
                    f"[TS]   - Sensor nível de bateria: FUNCIONAL "
                    f"(nível atual: {bat:.1f}%)"
                )

        # Antena
        self.logger.info(
            f"[TS]   - Sensor antena de comunicações: "
            f"{'FUNCIONAL' if antenna_ok else 'NÃO FUNCIONAL'}"
        )

        # Resumo
        if temp_ok and bat_ok and tires_ok and antenna_ok:
            self.logger.info("[TS] Todos os sensores do rover estão funcionais.")
        else:
            self.logger.warning("[TS] Um ou mais sensores do rover estão com problemas.")

    
    async def handle_client(self, reader, writer):
        addr = writer.get_extra_info('peername')
        self.logger.info(f"[TS] Nova conexão de telemetria de {addr}")

        rover_id = None
        SIZE = TelemetryMessage.SIZE

        try:
            while True:
                # lê exatamente uma mensagem completa (tamanho fixo)
                data = await reader.readexactly(TelemetryMessage.SIZE)
                
                telemetry = TelemetryMessage.from_bytes(data)
                rover_id = telemetry.rover_id

                if rover_id not in self.clients:
                    self.clients[rover_id] = writer

                sensor_status, sensor_details = self._process_sensors(telemetry)

                self.telemetry_manager.update_telemetry(telemetry)
                if self.mothership.show_telemetry:
                    self.logger.info(
                        f"[TS] Telemetria de {telemetry.rover_id} | "
                        f"Pos: ({telemetry.position.x:.2f}, {telemetry.position.y:.2f}, {telemetry.position.z:.2f}) | "
                        f"Bat: {telemetry.battery:.1f}% | "
                        f"Estado: {telemetry.state.name} | "
                        f"Temp: {telemetry.temperature:.1f}ºC | "
                        f"Vel: ({telemetry.speed.x:.2f}, {telemetry.speed.y:.2f}, {telemetry.speed.z:.2f})"
                    )
                    self._maybe_report_sensors(telemetry, sensor_status, sensor_details)
                
        except asyncio.IncompleteReadError:
            self.logger.info(f"[TS] Rover {rover_id} desconectou (EOF)")
        except Exception as e:
            self.logger.error(f"Erro ao processar telemetria de {addr}: {e}")
        finally:
            if rover_id and rover_id in self.clients:
                del self.clients[rover_id]
                self.telemetry_manager.remove_rover(rover_id)

            writer.close()
            await writer.wait_closed()
            self.logger.info(f"[TS] Conexão com {addr} encerrada")
    
    async def stop_server(self):
        """Para servidor"""
        if self.server:
            self.server.close()
            await self.server.wait_closed()

class TelemetryStreamClient:
    def __init__(self, rover_id: str, mothership_host: str, mothership_port: int = None):
        self.rover_id = rover_id
        self.mothership_host = mothership_host
        self.mothership_port = mothership_port or ProtocolConfig.TELEMETRY_STREAM_PORT
        self.logger = logging.getLogger(__name__)
        self.writer = None
        self.reader = None
        self.connected = False
        
    async def connect(self):
        """Conecta ao servidor de telemetria"""
        try:
            self.reader, self.writer = await asyncio.open_connection(
                self.mothership_host, 
                self.mothership_port
            )
            self.connected = True
            self.logger.info(f"[TS] Conectado ao servidor de telemetria {self.mothership_host}:{self.mothership_port}")
            return True
        except Exception as e:
            self.logger.error(f"Erro ao conectar: {e}")
            return False
    
    async def send_telemetry(self, telemetry: TelemetryMessage):
        """Envia telemetria"""
        if not self.connected or not self.writer:
            return False
        
        try:
            data = telemetry.to_bytes()
            self.writer.write(data)
            await self.writer.drain()
            return True
        except Exception as e:
            self.logger.error(f"Erro ao enviar telemetria: {e}")
            self.connected = False
            return False
    
    async def start_periodic_telemetry(self, get_telemetry_func, interval: float = None):
        """Inicia envio periódico de telemetria"""
        interval = interval or ProtocolConfig.TELEMETRY_INTERVAL

        while self.connected:
            try:
                telemetry = get_telemetry_func()
                if telemetry:
                    await self.send_telemetry(telemetry)
                
                await asyncio.sleep(interval)
                
            except asyncio.CancelledError:
                break
            except Exception as e:
                self.logger.error(f"Erro no envio periódico: {e}")
                await asyncio.sleep(interval)
    
    async def disconnect(self):
        if self.writer:
            self.writer.close()
            await self.writer.wait_closed()
        self.connected = False
        self.logger.info("Desconectado do servidor de telemetria")