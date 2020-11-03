#!/usr/bin/env python3

import os
from pathlib import Path

CONFIG = Path.home() / Path("github/dotfiles/config.yaml")

def process(line: str) -> None:
    symlink = line.split(": ")[1]
    os.unlink(str(Path.home() / symlink))


if __name__ == "__main__":
    for line in CONFIG.read_text().split("\n"):
        if line:
            try:
                process(line)
            except FileNotFoundError:
                pass
    print("Done!")
