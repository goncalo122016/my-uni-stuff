import asyncio
import json
import logging
from typing import Set, Dict, Any, Optional
import weakref

from common.message_types import APIEvent


class EventManager:
    def __init__(self):
        self.subscribers: Set[weakref.ref] = set()
        self.logger = logging.getLogger(__name__)
        
    def subscribe(self, callback):
        """Subscribe a callback that expects JSON (dict or json string)."""
        self.subscribers.add(weakref.ref(callback))
    
    def unsubscribe(self, callback):
        """Remove subscriber"""
        to_remove = None
        for ref in self.subscribers:
            if ref() == callback:
                to_remove = ref
                break
        
        if to_remove:
            self.subscribers.remove(to_remove)
    
    def emit_event(self, event: APIEvent):
        """
        Emit event to local subscribers in JSON form.
        You can choose between:
            - event.to_json() -> JSON string
            - event.to_dict() -> Python dict
        JSON string is safer for consistency.
        """
        json_payload = event.to_json()

        dead_refs = set()

        for ref in self.subscribers.copy():
            callback = ref()
            if callback is None:
                dead_refs.add(ref)
            else:
                try:
                    callback(json_payload) 
                except Exception as e:
                    self.logger.error(f"Erro ao processar evento: {e}")

        self.subscribers -= dead_refs
    
    # Specific events

    def emit_telemetry_update(self, rover_id: str, telemetry_data: Dict[str, Any]):
        event = APIEvent(
            event="telemetry",
            rover_id=rover_id,
            data=telemetry_data
        )
        self.emit_event(event)
    
    def emit_mission_assigned(self, rover_id: str, mission_id: str, mission_data: Dict[str, Any]):
        event = APIEvent(
            event="mission_assigned",
            rover_id=rover_id,
            mission_id=mission_id,
            data=mission_data
        )
        self.emit_event(event)
    
    def emit_mission_progress(self, rover_id: str, mission_id: str, progress_data: Dict[str, Any]):
        event = APIEvent(
            event="mission_progress",
            rover_id=rover_id,
            mission_id=mission_id,
            data=progress_data
        )
        self.emit_event(event)
    
    def emit_mission_completed(self, rover_id: str, mission_id: str):
        event = APIEvent(
            event="mission_completed",
            rover_id=rover_id,
            mission_id=mission_id
        )
        self.emit_event(event)
    
    def emit_mission_aborted(self, rover_id: str, mission_id: str):
        event = APIEvent(
            event="mission_aborted",
            rover_id=rover_id,
            mission_id=mission_id
        )
        self.emit_event(event)
    
    def emit_low_battery(self, rover_id: str, battery_level: int):
        event = APIEvent(
            event="low_battery",
            rover_id=rover_id,
            data={"battery": battery_level}
        )
        self.emit_event(event)
    
    def emit_rover_state_change(self, rover_id: str, old_state: str, new_state: str):
        event = APIEvent(
            event="state_change",
            rover_id=rover_id,
            data={"old_state": old_state, "new_state": new_state}
        )
        self.emit_event(event)


# WebSocket JSON Support
try:
    from flask_socketio import SocketIO, emit
    
    class WebSocketEventManager(EventManager):
        def __init__(self, socketio: SocketIO):
            super().__init__()
            self.socketio = socketio

        def emit_event(self, event: APIEvent):
            """Emit JSON via WebSocket and subscribers."""
            super().emit_event(event)

            self.socketio.emit(
                'api_event',
                event.to_dict(),
                json=True
            )

except ImportError:
    WebSocketEventManager = EventManager
