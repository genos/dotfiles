#!/usr/bin/env python3
"""Uninstall my dotfiles"""

import os
from pathlib import Path

CONFIG = Path(__file__).parent / Path("config.yaml")


def process(line: str) -> None:
    """Remove all symlinks listed in the CONFIG"""
    os.unlink(str(Path.home() / line.split(": ")[1]))


if __name__ == "__main__":
    for LINE in CONFIG.read_text().split("\n"):
        if LINE:
            try:
                process(LINE)
            except FileNotFoundError:
                pass
    print("Done!")
