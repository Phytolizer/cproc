from cproc.lexer import Lexer
from cproc.opts import Opts


def _process_file(f: str) -> None:
    with open(f, "r") as fp:
        raw_contents = fp.read()

    for tok in Lexer(raw_contents):
        print(tok)


def process_files(opts: Opts) -> None:
    for f in opts.files:
        _process_file(f)
