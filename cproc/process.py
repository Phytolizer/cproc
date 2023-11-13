from pathlib import Path

from cproc.lexer import Lexer
from cproc.opts import Opts
from cproc.source_lines import SourceLines


def _process_file(f: Path) -> None:
    with f.open() as fp:
        raw_contents = fp.read()

    lines = SourceLines(f, raw_contents)
    for tok in Lexer(raw_contents):
        print(f"{lines.offset(tok.pos)}: {tok}")


def process_files(opts: Opts) -> None:
    for f in opts.files:
        _process_file(f)
