from dataclasses import dataclass

@dataclass
class ProtocolConfig:
    # Mission Link UDP
    MISSION_LINK_PORT = 5005
    
    # Telemetry Stream TCP
    TELEMETRY_STREAM_PORT = 5006
    
    # API HTTP
    API_HTTP_PORT = 5007
    
    # Reliability settings
    RELIABILITY_TIMEOUT = 2.0  # segundos
    RELIABILITY_MAX_RETRIES = 3
    
    # Telemetry settings
    TELEMETRY_INTERVAL = 5.0  # segundos
    
    # Progress reporting
    DEFAULT_PROGRESS_PERIOD = 2  # segundos