from argparse import ArgumentParser
from pathlib import Path

from cproc.opts import Opts


def handle_args() -> Opts:
    p = ArgumentParser(prog="cproc")
    p.add_argument(
        "file",
        metavar="FILE",
        nargs="+",
        help="Files to process",
        type=Path,
    )
    args = p.parse_args()

    return Opts(files=args.file)
