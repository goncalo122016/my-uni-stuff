from flask import Flask, request, Response
from flask_cors import CORS
import logging
import threading
import json
import time
import struct
from typing import Optional, Dict, Any

from .endpoints import create_api_endpoints
from .events import EventManager, WebSocketEventManager
from common.message_types import APIEvent

try:
    from flask_socketio import SocketIO
    SOCKETIO_AVAILABLE = True
except ImportError:
    SOCKETIO_AVAILABLE = False

class APIServer:
    def __init__(self, telemetry_manager, mission_manager, host: str = '0.0.0.0', port: int = 5007):
        self.host = host
        self.port = port
        self.telemetry_manager = telemetry_manager
        self.mission_manager = mission_manager
        self.logger = logging.getLogger(__name__)
        logging.getLogger("werkzeug").setLevel(logging.ERROR)
        CORS_ENABLED = True

        # Criar Flask app
        self.app = Flask(__name__, static_folder="ui", static_url_path="")
        self.app.config['SECRET_KEY'] = 'mothership_secret_key'
        CORS(self.app)

        # Log de requests (antes de handlers)
        @self.app.before_request
        def log_request():
            self.logger.info(f"[API] {request.remote_addr} -> {request.method} {request.path}")

        # Criar event manager
        if SOCKETIO_AVAILABLE:
            self.socketio = SocketIO(self.app, cors_allowed_origins="*")
            self.event_manager = WebSocketEventManager(self.socketio)
        else:
            self.socketio = None
            self.event_manager = EventManager()

        create_api_endpoints(self.app, telemetry_manager, mission_manager, self.event_manager)

        # Configurar callbacks
        self._setup_callbacks()

        self.server_thread = None
        self.running = False

    def _setup_callbacks(self):
        """Configura callbacks para eventos"""
        # Telemetry callbacks
        self.telemetry_manager.on_telemetry_received = self._on_telemetry_received
        self.telemetry_manager.on_low_battery = self._on_low_battery
        self.telemetry_manager.on_state_change = self._on_state_change

        # Mission callbacks
        self.mission_manager.on_mission_assigned = self._on_mission_assigned
        self.mission_manager.on_mission_progress = self._on_mission_progress
        self.mission_manager.on_mission_completed = self._on_mission_completed
        self.mission_manager.on_mission_aborted = self._on_mission_aborted

    def _on_telemetry_received(self, telemetry):
        telemetry_data = {
            "position": telemetry.position.to_dict(),
            "state": telemetry.state.name,
            "battery": telemetry.battery,
            "speed": telemetry.speed.to_dict(),
            "mission_id": telemetry.mission_id,
            "timestamp": telemetry.timestamp
        }
        self.event_manager.emit_telemetry_update(telemetry.rover_id, telemetry_data)

    def _on_low_battery(self, rover_id: str, battery_level: int):
        self.event_manager.emit_low_battery(rover_id, battery_level)

    def _on_state_change(self, rover_id: str, old_state, new_state):
        self.event_manager.emit_rover_state_change(rover_id, old_state.name, new_state.name)

    def _on_mission_assigned(self, mission):
        mission_data = {
            "area": mission.area.to_dict(),
            "tasks": mission.tasks,
            "duration": mission.duration,
            "progress_period": mission.progress_period
        }
        self.event_manager.emit_mission_assigned(mission.rover_id, mission.mission_id, mission_data)

    def _on_mission_progress(self, mission, position, battery):
        progress_data = {
            "progress": mission.progress,
            "position": position.to_dict(),
            "battery": battery
        }
        self.event_manager.emit_mission_progress(mission.rover_id, mission.mission_id, progress_data)

    def _on_mission_completed(self, mission):
        self.event_manager.emit_mission_completed(mission.rover_id, mission.mission_id)

    def _on_mission_aborted(self, mission):
        self.event_manager.emit_mission_aborted(mission.rover_id, mission.mission_id)

    def start_server(self):
        """Inicia servidor API (respostas binárias)"""
        self.running = True

        def run_server():
            if self.socketio:
                self.socketio.run(self.app, host=self.host, port=self.port, debug=False)
            else:
                self.app.run(host=self.host, port=self.port, debug=False)

        self.server_thread = threading.Thread(target=run_server, daemon=True)
        self.server_thread.start()

        self.logger.info(f"[API] API Server iniciado em http://{self.host}:{self.port}")
        if self.socketio:
            self.logger.info(f"[API] WebSocket disponível em ws://{self.host}:{self.port}")

    def stop_server(self):
        self.running = False
        # Flask não tem graceful shutdown built-in
