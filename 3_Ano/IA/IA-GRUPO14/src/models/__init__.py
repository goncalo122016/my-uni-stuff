try:
    from .vehicle import Vehicle, VehicleType, VehicleStatus, FuelType, Position
    from .request import TransportRequest, RequestStatus, RequestPriority, EnvironmentalPreference, Station
except ImportError:
    from models.vehicle import Vehicle, VehicleType, VehicleStatus, FuelType, Position
    from models.request import TransportRequest, RequestStatus, RequestPriority, EnvironmentalPreference, Station

__all__ = [
    'Vehicle', 'VehicleType', 'VehicleStatus', 'FuelType', 'Position',
    'TransportRequest', 'RequestStatus', 'RequestPriority', 'EnvironmentalPreference', 'Station'
]
