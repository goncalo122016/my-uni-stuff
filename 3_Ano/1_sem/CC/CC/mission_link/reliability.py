import asyncio
import logging
from typing import Dict, Optional, Callable
from dataclasses import dataclass
from common.protocol_config import ProtocolConfig
import time

@dataclass
class PendingMessage:
    packet: bytes
    timestamp: float
    retries: int
    callback: Optional[Callable] = None

class ReliabilityManager:
    def __init__(self, timeout: float = ProtocolConfig.RELIABILITY_TIMEOUT, max_retries: int = ProtocolConfig.RELIABILITY_MAX_RETRIES):
        self.timeout = timeout
        self.max_retries = max_retries
        self.pending_messages: Dict[int, PendingMessage] = {}
        self.seq_counter = 0
        self.logger = logging.getLogger(__name__)
        self.transport = None
        
    def set_transport(self, transport):
        self.transport = transport
    
    def get_next_seq(self) -> int:
        self.seq_counter += 1
        return self.seq_counter
    
    def send_reliable(self, packet: bytes, addr: tuple, seq: int, callback: Optional[Callable] = None):
        
        self.pending_messages[seq] = PendingMessage(
            packet=packet,
            timestamp=time.time(),
            retries=0,
            callback=callback
        )
        
        self.transport.sendto(packet, addr)
        asyncio.create_task(self._handle_timeout(seq, addr))
    
    async def _handle_timeout(self, seq: int, addr: tuple):
        await asyncio.sleep(self.timeout)

        if seq not in self.pending_messages:
            return  # ACK chegou a tempo
        
        pending = self.pending_messages[seq]

        if pending.retries >= self.max_retries:
            self.logger.error(f"[RELIABLE] Falhou entrega seq={seq}")
            if pending.callback:
                pending.callback(False, seq)
            del self.pending_messages[seq]
            return
        
        # Retransmissão
        pending.retries += 1
        pending.timestamp = time.time()
        self.transport.sendto(pending.packet, addr)
        self.logger.warning(f"[RELIABLE] Retransmitindo seq {seq} (tentativa {pending.retries})")
        
        asyncio.create_task(self._handle_timeout(seq, addr))
    
    def handle_ack(self, ack_seq: int):
        if ack_seq in self.pending_messages:
            pending = self.pending_messages[ack_seq]
            if pending.callback:
                pending.callback(True, ack_seq)
            del self.pending_messages[ack_seq]
            return True
        return False