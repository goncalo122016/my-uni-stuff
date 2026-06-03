from sys import flags
from flask import Flask, request, Response
from flask_cors import CORS
import logging
import json
import struct
import time
from common.message_types import APIEvent

def create_api_endpoints(app: Flask, telemetry_manager, mission_manager, event_manager) -> Flask:
    CORS(app)
    logger = logging.getLogger(__name__)

    # ROVERS SNAPSHOT
    @app.route('/rovers', methods=['GET'])
    def get_rovers():
        try:
            rovers_data = {}
            rovers_telemetry = telemetry_manager.get_all_rovers()

            for rover_id, telemetry in rovers_telemetry.items():
                mission = mission_manager.get_rover_mission(rover_id)
                rovers_data[rover_id] = {
                    "rover_id": rover_id,
                    "position": telemetry.position.to_dict(),
                    "state": telemetry.state.name,
                    "battery": telemetry.battery,
                    "speed": telemetry.speed.to_dict(),
                    "temperature": telemetry.temperature,
                    "mission_id": mission.mission_id if mission else None,
                    "last_update": telemetry.timestamp
                }

            evt = APIEvent(event="rovers_snapshot", data={"rovers": rovers_data})
            return Response(evt.to_json(), mimetype='application/json')

        except Exception as e:
            logger.error(f"Erro ao obter rovers: {e}")
            evt = APIEvent(event="error", data={"message": str(e)})
            return Response(evt.to_json(), mimetype='application/json'), 500

    # SINGLE ROVER
    @app.route('/rovers/<rover_id>', methods=['GET'])
    def get_rover(rover_id: str):
        try:
            telemetry = telemetry_manager.get_rover_telemetry(rover_id)
            if not telemetry:
                evt = APIEvent(event="error", data={"message": "Rover not found"})
                return Response(evt.to_json(), mimetype='application/json'), 404

            mission = mission_manager.get_rover_mission(rover_id)
            data = {
                "rover_id": rover_id,
                "position": telemetry.position.to_dict(),
                "state": telemetry.state.name,
                "battery": telemetry.battery,
                "speed": telemetry.speed.to_dict(),
                "mission_id": mission.mission_id if mission else None,
                "timestamp": telemetry.timestamp
            }
            evt = APIEvent(event="telemetry", rover_id=rover_id, data=data)
            return Response(evt.to_json(), mimetype='application/json')

        except Exception as e:
            logger.error(f"Erro ao obter rover {rover_id}: {e}")
            evt = APIEvent(event="error", data={"message": str(e)})
            return Response(evt.to_json(), mimetype='application/json'), 500

    # GET ALL MISSIONS
    @app.route('/missions', methods=['GET'])
    def get_missions():
        try:
            missions_data = {}
            missions = mission_manager.get_all_missions()

            for mission_id, mission in missions.items():
                missions_data[mission_id] = {
                    "mission_id": mission_id,
                    "rover_id": mission.rover_id,
                    "area": mission.area.to_dict(),
                    "tasks": mission.tasks,
                    "duration": mission.duration,
                    "progress": mission.progress,
                    "status": mission.status.value,
                    "start_time": mission.start_time,
                    "end_time": mission.end_time,
                    "created_time": mission.created_time
                }

            evt = APIEvent(event="missions_snapshot", data={"missions": missions_data})
            return Response(evt.to_json(), mimetype='application/json')

        except Exception as e:
            logger.error(f"Erro ao obter missões: {e}")
            evt = APIEvent(event="error", data={"message": str(e)})
            return Response(evt.to_json(), mimetype='application/json'), 500

    # GET SINGLE MISSION
    @app.route('/missions/<mission_id>', methods=['GET'])
    def get_mission(mission_id: str):
        try:
            mission = mission_manager.get_mission(mission_id)
            if not mission:
                evt = APIEvent(event="error", data={"message": "Mission not found"})
                return Response(evt.to_json(), mimetype='application/json'), 404

            data = {
                "mission_id": mission_id,
                "rover_id": mission.rover_id,
                "area": mission.area.to_dict(),
                "tasks": mission.tasks,
                "duration": mission.duration,
                "progress": mission.progress,
                "status": mission.status.value,
                "start_time": mission.start_time,
                "end_time": mission.end_time,
                "created_time": mission.created_time
            }
            evt = APIEvent(event="mission_progress", mission_id=mission_id, data=data)
            return Response(evt.to_json(), mimetype='application/json')

        except Exception as e:
            logger.error(f"Erro ao obter missão {mission_id}: {e}")
            evt = APIEvent(event="error", data={"message": str(e)})
            return Response(evt.to_json(), mimetype='application/json'), 500

    # SEND MISSION
    @app.route('/send-mission', methods=['POST'])
    def send_mission():
        try:
            data = request.get_json()
            required = ['rover_id', 'area', 'tasks', 'duration']

            for f in required:
                if f not in data:
                    evt = APIEvent(event="error", data={"message": f"Field {f} required"})
                    return Response(evt.to_json(), mimetype='application/json'), 400

            from common.message_types import Area
            a = data['area']
            area = Area(x1=a['x1'], y1=a['y1'], x2=a['x2'], y2=a['y2'])

            mission_id = mission_manager.create_mission(
                area=area,
                tasks=data['tasks'],
                duration=data['duration'],
                progress_period=data.get('progress_period', 120)
            )

            if mission_manager.assign_mission(mission_id, data['rover_id']):
                evt = APIEvent(
                    event="mission_assigned",
                    rover_id=data['rover_id'],
                    mission_id=mission_id,
                    data={"mission_id": mission_id, "message": "created_and_assigned"}
                )
                return Response(evt.to_json(), mimetype='application/json')
            else:
                evt = APIEvent(event="error", data={"message": "failed_assign"})
                return Response(evt.to_json(), mimetype='application/json'), 400

        except Exception as e:
            evt = APIEvent(event="error", data={"message": str(e)})
            return Response(evt.to_json(), mimetype='application/json'), 500

    # CANCEL MISSION
    @app.route('/missions/<mission_id>/cancel', methods=['POST'])
    def cancel_mission(mission_id: str):
        try:
            if mission_manager.cancel_mission(mission_id):
                evt = APIEvent(
                    event="mission_aborted",
                    mission_id=mission_id,
                    data={"mission_id": mission_id}
                )
                return Response(evt.to_json(), mimetype='application/json')
            else:
                evt = APIEvent(event="error", data={"message": "cancel_failed"})
                return Response(evt.to_json(), mimetype='application/json'), 400

        except Exception as e:
            logger.error(f"Erro ao cancelar missão {mission_id}: {e}")
            evt = APIEvent(event="error", data={"message": str(e)})
            return Response(evt.to_json(), mimetype='application/json'), 500
        
    # GET LAST TELEMETRY EVENTS (all rovers)
    @app.route('/telemetry/latest', methods=['GET'])
    def telemetry_latest():
        try:
            limit = request.args.get("limit", type=int)
            histories = telemetry_manager.get_all_rover_histories(limit)
    
            def normalize_timestamp(ts):
                # Se já é número -> OK
                if isinstance(ts, (int, float)):
                    return int(ts)
    
                # Se for string no formato "dd/mm/yyyy HH:MM:SS"
                try:
                    from datetime import datetime
                    dt = datetime.strptime(ts, "%d/%m/%Y %H:%M:%S")
                    return int(dt.timestamp())
                except:
                    return 0
    
            def convert(t):
                flags = getattr(t, "sensor_flags", 0)

                sensor_status = {
                    "temperature": not bool(flags & (1 << 0)),
                    "battery": not bool(flags & (1 << 1)),
                    "tires": not bool(flags & (1 << 2)),
                    "antenna": not bool(flags & (1 << 3)),
                }

                return {
                    "rover_id": t.rover_id,
                    "position": t.position.to_dict(),
                    "state": t.state.name,
                    "battery": t.battery,
                    "speed": t.speed.to_dict(),
                    "temperature": t.temperature,
                    "sensor_status": sensor_status,
                    "timestamp": normalize_timestamp(t.timestamp)
                }
    
            response = {
                rid: [
                    convert(t)
                    for t in sorted(
                        hist,
                        key=lambda x: normalize_timestamp(x.timestamp),
                        reverse=True
                    )[:5]
                ]
                for rid, hist in histories.items()
            }
    
            evt = APIEvent(event="telemetry_history", data=response)
            return Response(evt.to_json(), mimetype="application/json")
    
        except Exception as e:
            evt = APIEvent(event="error", data={"message": str(e)})
            return Response(evt.to_json(), mimetype="application/json"), 500

    # HEALTH CHECK
    @app.route('/health', methods=['GET'])
    def health_check():
        try:
            payload = {
                "service": "MotherShip API",
                "active_rovers": len(telemetry_manager.get_all_rovers()),
                "active_missions": len(mission_manager.get_active_missions())
            }
            evt = APIEvent(event="health", data=payload)
            return Response(evt.to_json(), mimetype='application/json')

        except Exception as e:
            evt = APIEvent(event="error", data={"message": str(e)})
            return Response(evt.to_json(), mimetype='application/json'), 500

    return app
