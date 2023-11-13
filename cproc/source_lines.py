from bisect import bisect_right
from dataclasses import dataclass
from os import PathLike
from pathlib import PurePath


@dataclass(kw_only=True, frozen=True)
class SourcePos:
    filename: str
    row: int
    col: int

    def __str__(self) -> str:
        return f"{self.filename}:{self.row}:{self.col}"


class SourceLines:
    filename: PurePath
    line_starts: list[int]

    def __init__(self, filename: PathLike[str], source: str) -> None:
        self.filename = PurePath(filename)
        self.line_starts = [0]
        for i, c in enumerate(source):
            if c == "\n":
                self.line_starts.append(i + 1)

    def offset(self, ofs: int) -> SourcePos:
        # binary search
        i = bisect_right(self.line_starts, ofs)
        if i != 0:
            start = self.line_starts[i - 1]
            return SourcePos(filename=self.filename.name, row=i, col=ofs - start + 1)
        raise IndexError("offset out of bounds")
