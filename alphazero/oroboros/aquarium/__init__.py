"""Oroboros Aquarium: persistent evolving population of game-organisms.

Run:
  python -m alphazero.oroboros.aquarium --forever --workers 4
"""

# Re-export everything from the module file
from alphazero.oroboros._aquarium import Aquarium, Organism, main

__all__ = ["Aquarium", "Organism", "main"]
