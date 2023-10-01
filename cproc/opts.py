from dataclasses import dataclass
from pathlib import Path


@dataclass
class Opts:
    files: list[Path]
