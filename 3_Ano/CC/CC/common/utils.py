import time
import json
import logging
import socket
import sys
from typing import Any, Dict, Optional
from datetime import datetime

def setup_logging(name: str, level: int = logging.INFO) -> logging.Logger:
    """Configura logging (força impressão no stdout, mesmo em asyncio)"""
    logger = logging.getLogger(name)
    logger.setLevel(level)

    if logger.hasHandlers():
        logger.handlers.clear()

    handler = logging.StreamHandler(sys.stdout)
    formatter = logging.Formatter(
        '%(asctime)s %(message)s',
        datefmt='%H:%M:%S'
    )
    handler.setFormatter(formatter)

    logger.addHandler(handler)

    logging.basicConfig(level=level, handlers=[handler], force=True)

    logger.propagate = False

    return logger

def calculate_distance_2d(pos1: tuple, pos2: tuple) -> float:
    """Calcula distância 2D entre duas posições"""
    return ((pos1[0] - pos2[0]) ** 2 + (pos1[1] - pos2[1]) ** 2) ** 0.5

def calculate_distance_3d(pos1: tuple, pos2: tuple) -> float:
    """Calcula distância 3D entre duas posições"""
    return ((pos1[0] - pos2[0]) ** 2 + (pos1[1] - pos2[1]) ** 2 + (pos1[2] - pos2[2]) ** 2) ** 0.5