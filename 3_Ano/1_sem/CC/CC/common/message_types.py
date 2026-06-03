import struct
from enum import Enum
from dataclasses import dataclass
from typing import Optional, Dict, Any, List
import json
import time

class MissionMessageType(Enum):
    MISSION_HELLO = 1
    MISSION_REQUEST = 2
    MISSION_ASSIGN = 3
    MISSION_ACKNOWLEDGMENT = 4
    MISSION_PROGRESS = 5
    MISSION_COMPLETE = 6
    MISSION_ABORT = 7
    MISSION_CANCEL = 8
    MISSION_PAUSED = 9

class RoverState(Enum):
    IDLE = 0
    IN_MISSION = 1
    CHARGING = 2
    ERROR = 3

    def to_str(self):
        return {
            0: "idle",
            1: "in_mission",
            2: "charging",
            3: "error"
        }[self.value]
    
    def from_str(s: str) -> "RoverState":
        mapping = {
            "idle": RoverState.IDLE,
            "in_mission": RoverState.IN_MISSION,
            "charging": RoverState.CHARGING,
            "error": RoverState.ERROR,
        }
        return mapping[s]

@dataclass
class Position:
    x: float
    y: float
    z: float
    def to_dict(self):
        return {"x": self.x, "y": self.y, "z": self.z}

@dataclass
class Area:
    x1: float
    y1: float
    x2: float
    y2: float
    def to_dict(self):
        return {"x1": self.x1, "y1": self.y1, "x2": self.x2, "y2": self.y2}

@dataclass
class Speed:
    x: float
    y: float
    z: float
    
    def to_dict(self):
        return {"x": self.x, "y": self.y, "z": self.z}

# MissionLink Message
@dataclass
class MissionMessage:
    type: MissionMessageType
    seq: int
    ack: Optional[int] = 0
    mission_id: Optional[str] = None
    rover_id: Optional[str] = None
    area: Optional[Area] = None
    tasks: Optional[str] = None
    duration: Optional[int] = 0
    progress_period: Optional[int] = 0
    progress: Optional[float] = 0.0
    position: Optional[Position] = None
    battery: Optional[int] = 0
    timestamp: Optional[str] = None

    def __post_init__(self):
        if self.timestamp is None:
            self.timestamp = time.strftime("%d/%m/%Y %H:%M:%S")

    _PAYLOAD_SIZE = 140  # bytes for fixed payload
    _MISSION_ID_SIZE = 8
    _ROVER_ID_SIZE = 8
    _TASKS_SIZE = 64

    def to_bytes(self) -> bytes:
        """
        Serialização binária fixa.
        Estrutura:
          header: type(1) | seq(4) | ack(4)
          payload (128):
            mission_id(8) | rover_id(8) | area(4*float) | tasks(64) |
            duration(4) | progress_period(4) | progress(4 float) | battery(1) | padding
          checksum: 1 byte (sum % 256)
        """
        # header
        type_id = int(self.type.value)  # already integer values in enum mapping
        seq = int(self.seq or 0)
        ack = int(self.ack or 0)

        header = struct.pack("!BII", type_id, seq, ack)  # B=1byte, I=4bytes

        # payload build
        # mission_id, rover_id (ascii padded)
        mid = (self.mission_id or "").encode('ascii', errors='ignore')[:self._MISSION_ID_SIZE]
        mid = mid.ljust(self._MISSION_ID_SIZE, b'\x00')

        rid = (self.rover_id or "").encode('ascii', errors='ignore')[:self._ROVER_ID_SIZE]
        rid = rid.ljust(self._ROVER_ID_SIZE, b'\x00')

        # area floats
        ax1 = float(self.area.x1) if (self.area and hasattr(self.area, 'x1')) else 0.0
        ay1 = float(self.area.y1) if (self.area and hasattr(self.area, 'y1')) else 0.0
        ax2 = float(self.area.x2) if (self.area and hasattr(self.area, 'x2')) else 0.0
        ay2 = float(self.area.y2) if (self.area and hasattr(self.area, 'y2')) else 0.0
        area_bytes = struct.pack("!ffff", ax1, ay1, ax2, ay2)

        # tasks
        tasks_bytes = (self.tasks or "").encode('utf-8', errors='ignore')[:self._TASKS_SIZE]
        tasks_bytes = tasks_bytes.ljust(self._TASKS_SIZE, b'\x00')

        # duration, progress_period, progress, battery
        duration = int(self.duration or 0)
        progress_period = int(self.progress_period or 0)
        progress = float(self.progress or 0.0)
        battery = int(self.battery or 0) & 0xFF
        posx = float(self.position.x) if (self.position) else 0.0
        posy = float(self.position.y) if (self.position) else 0.0
        posz = float(self.position.z) if (self.position) else 0.0
        
        pos_bytes = struct.pack("!fff", posx, posy, posz)

        tail = struct.pack("!IIfB", duration, progress_period, progress, battery) + pos_bytes

        # assemble payload (ensure exact size)
        payload = mid + rid + area_bytes + tasks_bytes + tail
        # pad payload to _PAYLOAD_SIZE
        if len(payload) < self._PAYLOAD_SIZE:
            payload = payload.ljust(self._PAYLOAD_SIZE, b'\x00')
        else:
            payload = payload[:self._PAYLOAD_SIZE]

        # checksum: simple sum modulo 256 over header+payload
        chk = (sum(header) + sum(payload)) % 256
        packet = header + payload + struct.pack("!B", chk)
        return packet

    @classmethod
    def from_bytes(cls, data: bytes) -> 'MissionMessage':
        """
        Parse bytes according to the format defined in to_bytes.
        Returns MissionMessage or raises ValueError on checksum/type errors.
        """
        if len(data) < 1 + 4 + 4 + cls._PAYLOAD_SIZE + 1:
            raise ValueError("Mensagem demasiado pequena")

        # split
        header = data[:9]
        payload = data[9:9 + cls._PAYLOAD_SIZE]
        chk_byte = data[9 + cls._PAYLOAD_SIZE]

        # verify checksum
        expected = (sum(header) + sum(payload)) % 256
        if chk_byte != expected:
            raise ValueError(f"Checksum inválido (got {chk_byte}, expected {expected})")

        type_id, seq, ack = struct.unpack("!BII", header)

        # mission_id, rover_id
        off = 0
        mid_raw = payload[off:off + cls._MISSION_ID_SIZE]; off += cls._MISSION_ID_SIZE
        rid_raw = payload[off:off + cls._ROVER_ID_SIZE]; off += cls._ROVER_ID_SIZE

        mid = mid_raw.split(b'\x00', 1)[0].decode('ascii', errors='ignore') or None
        rid = rid_raw.split(b'\x00', 1)[0].decode('ascii', errors='ignore') or None

        # area floats
        ax1, ay1, ax2, ay2 = struct.unpack_from("!ffff", payload, off); off += 16
        area = Area(ax1, ay1, ax2, ay2)

        # tasks
        tasks_raw = payload[off:off + cls._TASKS_SIZE]; off += cls._TASKS_SIZE
        tasks = tasks_raw.split(b'\x00', 1)[0].decode('utf-8', errors='ignore') or None

        # tail
        duration, progress_period, progress = struct.unpack_from("!IIf", payload, off); off += 12
        battery = struct.unpack_from("!B", payload, off)[0]; off += 1

        posx, posy, posz = struct.unpack_from("!fff", payload, off)
        off += 12
        position = Position(posx, posy, posz)

        # create message
        type_enum = MissionMessageType(type_id)
        return cls(
            type=type_enum,
            seq=int(seq),
            ack=int(ack) if ack != 0 else None,
            mission_id=mid,
            rover_id=rid,
            area=area,
            tasks=tasks,
            duration=int(duration),
            progress_period=int(progress_period),
            progress=float(progress),
            battery=int(battery),
            position=position
        )

# Telemetry Message
@dataclass
class TelemetryMessage:
    rover_id: str
    mission_id: Optional[str]
    state: RoverState
    battery: int
    temperature: float
    position: Position
    speed: Speed
    sensor_flags: int = 0
    timestamp: Optional[str] = None  # apenas humano, não vai no binário

    MAGIC = 0xAA55
    SIZE = 52

    # Novo formato:
    # MAGIC(H) | SIZE(H) | rover_id(8s) | mission_id(8s)
    # | state(B) | battery(B) | temperature(f)
    # | pos.xyz (fff) | speed.xyz (fff) | checksum(B)
    _FMT = "!H H 8s 8s B B f fff fff B B"

    def __post_init__(self):
        if self.timestamp is None:
            self.timestamp = time.strftime("%d/%m/%Y %H:%M:%S")

    def to_bytes(self):
        rid = self.rover_id.encode()[:8].ljust(8, b"\x00")
        mid = (self.mission_id or "").encode()[:8].ljust(8, b"\x00")

        # pack todos os campos exceto checksum
        pack_fmt_base = self._FMT.rsplit(" ", 1)[0]

        base = struct.pack(
            pack_fmt_base,
            self.MAGIC,
            self.SIZE,
            rid,
            mid,
            self._state_to_byte(self.state),
            int(self.battery) & 0xFF,
            float(self.temperature),
            float(self.position.x),
            float(self.position.y),
            float(self.position.z),
            float(self.speed.x),
            float(self.speed.y),
            float(self.speed.z),
            int(self.sensor_flags) & 0xFF
        )

        chk = sum(base) % 256
        return base + struct.pack("!B", chk)

    @classmethod
    def from_bytes(cls, data: bytes):
        if len(data) != cls.SIZE:
            raise ValueError(f"Tamanho inválido (esperado {cls.SIZE}, recebido {len(data)})")

        # unpack incluindo checksum
        unpacked = struct.unpack(cls._FMT, data)
        (*fields, checksum) = unpacked

        # base = tudo exceto checksum final
        base = data[:-1]
        if sum(base) % 256 != checksum:
            raise ValueError("Checksum inválido")

        (
            magic, size,
            rid, mid,
            state_byte,
            battery,
            temp,
            px, py, pz,
            sx, sy, sz,
            sensor_flags
        ) = fields

        return cls(
            rover_id=rid.split(b"\x00", 1)[0].decode(),
            mission_id=mid.split(b"\x00", 1)[0].decode() or None,
            state=cls._byte_to_state(state_byte),
            battery=int(battery),
            temperature=float(temp),
            position=Position(px, py, pz),
            speed=Speed(sx, sy, sz),
            sensor_flags = int(sensor_flags)
        )

    @staticmethod
    def _state_to_byte(state: RoverState) -> int:
        mapping = {
            RoverState.IDLE: 0,
            RoverState.IN_MISSION: 1,
            RoverState.CHARGING: 2,
            RoverState.ERROR: 3
        }
        if isinstance(state, RoverState):
            return mapping[state]
        return int(state) & 0xFF

    @staticmethod
    def _byte_to_state(b: int) -> RoverState:
        mapping = {
            0: RoverState.IDLE,
            1: RoverState.IN_MISSION,
            2: RoverState.CHARGING,
            3: RoverState.ERROR
        }
        return mapping.get(b, RoverState.IDLE)

@dataclass
class APIEvent:
    """
    Evento API em JSON simples para transporte via HTTP:

    {
        "event": "telemetry",
        "rover_id": "R1",
        "mission_id": "M1",
        "timestamp": 1716820000,
        "data": {...}
    }
    """

    EVENT_CODES = {
        "unknown": 0,
        "telemetry": 1,
        "low_battery": 2,
        "state_change": 3,
        "mission_assigned": 4,
        "mission_progress": 5,
        "mission_completed": 6,
        "mission_aborted": 7,
        "rovers_snapshot": 10,
        "missions_snapshot": 11,
        "health": 12,
        "ok": 200,
        "error": 201
    }

    def __init__(self,
                 event: str,
                 rover_id: Optional[str] = None,
                 mission_id: Optional[str] = None,
                 data: Optional[Dict[str, Any]] = None,
                 timestamp: Optional[int] = None):
        self.event = event
        self.rover_id = rover_id or ""
        self.mission_id = mission_id or ""
        self.data = data or {}
        self.timestamp = int(timestamp or time.time())

    def to_json(self) -> bytes:
        payload = {
            "event": self.event,
            "event_code": self.EVENT_CODES.get(self.event, 0),
            "rover_id": self.rover_id,
            "mission_id": self.mission_id,
            "timestamp": self.timestamp,
            "data": self.data,
        }
        return json.dumps(payload).encode("utf-8")

    @classmethod
    def from_json(cls, raw: bytes):
        try:
            obj = json.loads(raw.decode("utf-8"))
        except Exception as e:
            raise ValueError(f"Invalid JSON event: {e}")

        return cls(
            event=obj.get("event", "unknown"),
            rover_id=obj.get("rover_id", ""),
            mission_id=obj.get("mission_id", ""),
            data=obj.get("data", {}),
            timestamp=obj.get("timestamp", int(time.time()))
        )
