"""
city_graph.py - Arquivo de compatibilidade que re-exporta classes modularizadas

Nota: Este arquivo mantém a compatibilidade com código antigo que importa direto daqui.
As classes foram movidas para:
- models.py: Node, Edge
- graph.py: CityGraph

Preferir importar de graph.__init__.py: `from graph import CityGraph, Node, Edge`
"""

# Re-exportar dos novos módulos para compatibilidade
try:
    from .models import Node, Edge
    from .graph import CityGraph
except ImportError:
    from graph.models import Node, Edge
    from graph.graph import CityGraph

__all__ = ['CityGraph', 'Node', 'Edge']
