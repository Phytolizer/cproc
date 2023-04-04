from enum import Enum, auto
from collections import defaultdict
from dataclasses import dataclass


class TokenKind(Enum):
    # Something unrecognizable
    ERROR = auto()

    # Marker tokens
    EOF = auto()

    # Standard C tokens:
    # Keywords
    KW_AUTO = auto()
    KW_BREAK = auto()
    KW_CASE = auto()
    KW_CHAR = auto()
    KW_CONST = auto()
    KW_CONTINUE = auto()
    KW_DEFAULT = auto()
    KW_DO = auto()
    KW_DOUBLE = auto()
    KW_ELSE = auto()
    KW_ENUM = auto()
    KW_EXTERN = auto()
    KW_FLOAT = auto()
    KW_FOR = auto()
    KW_GOTO = auto()
    KW_IF = auto()
    KW_INLINE = auto()
    KW_INT = auto()
    KW_LONG = auto()
    KW_REGISTER = auto()
    KW_RESTRICT = auto()
    KW_RETURN = auto()
    KW_SHORT = auto()
    KW_SIGNED = auto()
    KW_SIZEOF = auto()
    KW_STATIC = auto()
    KW_STRUCT = auto()
    KW_SWITCH = auto()
    KW_TYPEDEF = auto()
    KW_UNION = auto()
    KW_UNSIGNED = auto()
    KW_VOID = auto()
    KW_VOLATILE = auto()
    KW_WHILE = auto()

    # "underscore" keywords
    KWU_ALIGNAS = auto()
    KWU_ALIGNOF = auto()
    KWU_ATOMIC = auto()
    KWU_BOOL = auto()
    KWU_COMPLEX = auto()
    KWU_GENERIC = auto()
    KWU_IMAGINARY = auto()
    KWU_NORETURN = auto()
    KWU_STATIC_ASSERT = auto()
    KWU_THREAD_LOCAL = auto()

    # Constants
    C_DECIMAL = auto()
    C_OCTAL = auto()
    C_HEX = auto()

    C_DECIMAL_FLOAT = auto()
    C_HEX_FLOAT = auto()

    C_CHAR_LIT = auto()
    C_STRING_LIT = auto()

    # Punctuators
    P_LBRACK = auto()
    P_RBRACK = auto()
    P_LPAREN = auto()
    P_RPAREN = auto()
    P_LBRACE = auto()
    P_RBRACE = auto()
    P_DOT = auto()
    P_ARROW = auto()
    P_PLUS_PLUS = auto()
    P_MINUS_MINUS = auto()
    P_AMP = auto()
    P_STAR = auto()
    P_PLUS = auto()
    P_MINUS = auto()
    P_TILDE = auto()
    P_BANG = auto()
    P_SLASH = auto()
    P_PERCENT = auto()
    P_LESS_LESS = auto()
    P_GREATER_GREATER = auto()
    P_LESS = auto()
    P_GREATER = auto()
    P_LESS_EQUAL = auto()
    P_GREATER_EQUAL = auto()
    P_EQUAL_EQUAL = auto()
    P_BANG_EQUAL = auto()
    P_CARET = auto()
    P_PIPE = auto()
    P_AMP_AMP = auto()
    P_PIPE_PIPE = auto()
    P_QUESTION = auto()
    P_COLON = auto()
    P_SEMI = auto()
    P_ELLIPSIS = auto()
    P_EQUAL = auto()
    P_STAR_EQUAL = auto()
    P_SLASH_EQUAL = auto()
    P_PERCENT_EQUAL = auto()
    P_PLUS_EQUAL = auto()
    P_MINUS_EQUAL = auto()
    P_LESS_LESS_EQUAL = auto()
    P_GREATER_GREATER_EQUAL = auto()
    P_AMP_EQUAL = auto()
    P_CARET_EQUAL = auto()
    P_PIPE_EQUAL = auto()
    P_COMMA = auto()
    # Note: '#' and '##' omitted intentionally

    # Digraphs
    P_LESS_COLON = auto()
    P_COLON_GREATER = auto()
    P_LESS_PERCENT = auto()
    P_PERCENT_GREATER = auto()
    # digraphs for '#' and '##' omitted

    # Other standard token types
    IDENT = auto()

    # Extended tokens for cproc
    KW_MACRO = auto()
    KW_IMPORT = auto()
    KW_AS = auto()

    def is_kw(self) -> bool:
        return self.name.startswith("KW")

    def is_punct(self) -> bool:
        return self.name.startswith("P_")

    def is_const(self) -> bool:
        return self.name.startswith("C_")

    def resolve_digraph(self) -> "TokenKind":
        if self == self.P_LESS_COLON:
            return self.P_LBRACK
        elif self == self.P_COLON_GREATER:
            return self.P_RBRACK
        elif self == self.P_LESS_PERCENT:
            return self.P_LBRACE
        elif self == self.P_PERCENT_GREATER:
            return self.P_RBRACE
        else:
            # not a digraph
            return self


def _gen_keywords() -> dict[str, TokenKind]:
    result: dict[str, TokenKind] = {}
    for kind in TokenKind:
        if not kind.is_kw():
            continue
        if kind.name.startswith("KW_"):
            # e.g. int, sizeof
            text = kind.name[3:].lower()
        elif kind.name.startswith("KWU_"):
            # e.g. _Static_assert, _Alignof
            text = "_" + kind.name[4].upper() + kind.name[5:].lower()
        else:
            assert False, "invalid token kind name"
        result[text] = kind
    return result


_KEYWORDS: defaultdict[str, TokenKind] = defaultdict(
    lambda: TokenKind.IDENT, _gen_keywords()
)


def ident_kind(ident: str) -> TokenKind:
    return _KEYWORDS[ident]


@dataclass(kw_only=True)
class Token:
    kind: TokenKind
    pos: int
    text: str
