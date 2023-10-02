from collections.abc import Callable

from cproc.token import Token, TokenKind


def _is_oct_digit(ch: str) -> bool:
    return ch.isdigit() and int(ch) in range(8)


def _is_hex_digit(ch: str) -> bool:
    return ch.isdigit() or (ch >= "A" and ch <= "F") or (ch >= "a" and ch <= "f")


_PUNCTUATORS: dict[str, TokenKind] = {
    "[": TokenKind.P_LBRACK,
    "]": TokenKind.P_RBRACK,
    "(": TokenKind.P_LPAREN,
    ")": TokenKind.P_RPAREN,
    "{": TokenKind.P_LBRACE,
    "}": TokenKind.P_RBRACE,
    ".": TokenKind.P_DOT,
    "->": TokenKind.P_ARROW,
    "++": TokenKind.P_PLUS_PLUS,
    "--": TokenKind.P_MINUS_MINUS,
    "&": TokenKind.P_AMP,
    "*": TokenKind.P_STAR,
    "+": TokenKind.P_PLUS,
    "-": TokenKind.P_MINUS,
    "~": TokenKind.P_TILDE,
    "!": TokenKind.P_BANG,
    "/": TokenKind.P_SLASH,
    "%": TokenKind.P_PERCENT,
    "<<": TokenKind.P_LESS_LESS,
    ">>": TokenKind.P_GREATER_GREATER,
    "<": TokenKind.P_LESS,
    ">": TokenKind.P_GREATER,
    "<=": TokenKind.P_LESS_EQUAL,
    ">=": TokenKind.P_GREATER_EQUAL,
    "==": TokenKind.P_EQUAL_EQUAL,
    "!=": TokenKind.P_BANG_EQUAL,
    "^": TokenKind.P_CARET,
    "|": TokenKind.P_PIPE,
    "&&": TokenKind.P_AMP_AMP,
    "||": TokenKind.P_PIPE_PIPE,
    "?": TokenKind.P_QUESTION,
    ":": TokenKind.P_COLON,
    ";": TokenKind.P_SEMI,
    "...": TokenKind.P_ELLIPSIS,
    "=": TokenKind.P_EQUAL,
    "*=": TokenKind.P_STAR_EQUAL,
    "/=": TokenKind.P_SLASH_EQUAL,
    "%=": TokenKind.P_PERCENT_EQUAL,
    "+=": TokenKind.P_PLUS_EQUAL,
    "-=": TokenKind.P_MINUS_EQUAL,
    "<<=": TokenKind.P_LESS_LESS_EQUAL,
    ">>=": TokenKind.P_GREATER_GREATER_EQUAL,
    "&=": TokenKind.P_AMP_EQUAL,
    "^=": TokenKind.P_CARET_EQUAL,
    "|=": TokenKind.P_PIPE_EQUAL,
    ",": TokenKind.P_COMMA,
    "<:": TokenKind.P_LESS_COLON,
    ":>": TokenKind.P_COLON_GREATER,
    "<%": TokenKind.P_LESS_PERCENT,
    "%>": TokenKind.P_PERCENT_GREATER,
}

_PUNCTUATORS_START = {k[0] for k in _PUNCTUATORS}

_PUNCTUATORS_NEXT = {
    c: sorted(
        (k for k in _PUNCTUATORS if k.startswith(c)),
        key=len,
        reverse=True,
    )
    for c in _PUNCTUATORS_START
}


class Lexer:
    def __init__(self, src: str) -> None:
        self._src = src

    def __iter__(self) -> "_Iterator":
        return self._Iterator(self)

    class _Iterator:
        def __init__(self, lex: "Lexer") -> None:
            self._lex = lex
            self._pos = 0
            self._was_eof = False

        def _move(self, /, n: int = 1) -> None:
            self._pos += n

        def _look(self, /, offset: int = 0) -> str | None:
            index = self._pos + offset
            if index < len(self._lex._src):
                return self._lex._src[index]
            return None

        def _look_ch(self, /, offset: int, exp: str) -> bool:
            return self._look_pred(offset, lambda ch: ch == exp)

        def _look_pred(self, /, offset: int, pred: Callable[[str], bool]) -> bool:
            ch = self._look(offset)
            if ch is None:
                return False
            return pred(ch)

        def _has_prefix(self, prefix: str, *, offset: int = 0) -> bool:
            if len(prefix) == 0:
                self._move(offset)
                return True
            for i, ch in enumerate(prefix, start=offset):
                if not self._look_ch(i, ch):
                    break
            else:
                return True
            return False

        def _token_has_prefix(self, prefix: str, *, start: int) -> bool:
            return self._lex._src[start:].startswith(prefix)

        def _match_prefix(self, prefix: str, *, offset: int = 0) -> bool:
            if self._has_prefix(prefix, offset=offset):
                self._move(len(prefix) + offset)
                return True
            return False

        def _skip_ws(self) -> None:
            # mini-lexer that also skips comments
            block_comment = False
            line_comment = False
            while True:
                assert not (block_comment and line_comment)
                if block_comment:
                    if self._match_prefix("*/"):
                        block_comment = False
                    else:
                        self._move()
                    continue
                if line_comment:
                    if self._look_ch(0, "\n"):
                        self._move()
                        line_comment = False
                    else:
                        self._move()
                    continue
                if self._look_pred(0, lambda ch: ch.isspace()):
                    self._move()
                    continue
                if self._match_prefix("/*"):
                    block_comment = True
                    continue
                if self._match_prefix("//"):
                    line_comment = True
                    continue
                break

        def _is_punct(self) -> bool:
            return self._look_pred(0, lambda ch: ch in _PUNCTUATORS_START)

        def _lex_punct(self) -> TokenKind:
            start = self._look()
            assert start is not None
            possibilities = _PUNCTUATORS_NEXT[start]
            for p in possibilities:
                if len(p) == 1:
                    self._move()
                    return _PUNCTUATORS[p]
                if self._match_prefix(p[1:], offset=1):
                    return _PUNCTUATORS[p].resolve_digraph()

            raise AssertionError("unreachable")

        def _is_string_start(self) -> bool:
            return (
                self._match_prefix('"')
                or self._match_prefix('u8"')
                or self._match_prefix('u"')
                or self._match_prefix('U"')
                or self._match_prefix('L"')
            )

        def _is_char_start(self) -> bool:
            return (
                self._match_prefix("'")
                or self._match_prefix("L'")
                or self._match_prefix("u'")
                or self._match_prefix("U'")
            )

        def _check_universal_char_name(self, *, offset: int = 0) -> int:
            first = self._look(offset)
            if first == "u":
                expected_count = 4
            elif first == "U":
                expected_count = 8
            else:
                return 0
            for i in range(expected_count):
                if not self._look_pred(i + offset + 1, _is_hex_digit):
                    return 0
            return expected_count

        def _lex_universal_char_name(self) -> bool:
            expected_count = self._check_universal_char_name()
            if expected_count == 0:
                return False
            self._move(expected_count + 1)
            return True

        def _match_escape_seq(self, *, code_unit_bytes: int = 1) -> bool:
            if (
                self._match_prefix("'")
                or self._match_prefix('"')
                or self._match_prefix("?")
                or self._match_prefix("\\")
                or self._match_prefix("a")
                or self._match_prefix("b")
                or self._match_prefix("f")
                or self._match_prefix("n")
                or self._match_prefix("r")
                or self._match_prefix("t")
                or self._match_prefix("v")
            ):
                return True

            if self._look_pred(0, _is_oct_digit):
                for i in range(3):
                    if not self._look_pred(i, _is_oct_digit):
                        self._move(i)
                        return True
                self._move(3)
                return True

            if self._match_prefix("x"):
                i = 0
                while self._look_pred(i, _is_hex_digit):
                    i += 1
                self._move(i)
                return i <= (code_unit_bytes * 2)

            if self._has_prefix("u") or self._has_prefix("U"):
                return self._lex_universal_char_name()

            return False

        def _lex_string(self, *, start: int) -> TokenKind:
            result = TokenKind.C_STRING_LIT
            code_unit_bytes = 1
            if self._token_has_prefix("U", start=start):
                code_unit_bytes = 4
            elif self._token_has_prefix("u8", start=start):
                code_unit_bytes = 1
            elif self._token_has_prefix("u", start=start):
                code_unit_bytes = 2
            elif self._token_has_prefix("L", start=start):
                # TODO(kyle): wchar_t size determination
                raise AssertionError("TODO: find a way to get wchar_t size")
            while True:
                if self._match_prefix("\n"):
                    return TokenKind.ERROR
                if self._match_prefix('"'):
                    return result
                if self._match_prefix("\\"):
                    if not self._match_escape_seq(code_unit_bytes=code_unit_bytes):
                        result = TokenKind.ERROR
                else:
                    self._move()

        def _lex_char(self, *, start: int) -> TokenKind:
            result = TokenKind.C_CHAR_LIT
            code_unit_bytes = 1
            if self._token_has_prefix("U", start=start):
                code_unit_bytes = 4
            elif self._token_has_prefix("u", start=start):
                code_unit_bytes = 2
            elif self._token_has_prefix("L", start=start):
                # TODO(kyle): wchar_t size determination
                raise AssertionError("TODO: find a way to get wchar_t size")
            while True:
                if self._match_prefix("\n"):
                    return TokenKind.ERROR
                if self._match_prefix("'"):
                    return result
                if self._match_prefix("\\"):
                    if not self._match_escape_seq(code_unit_bytes=code_unit_bytes):
                        result = TokenKind.ERROR
                else:
                    self._move()

        def _is_ident_start(self) -> bool:
            if self._look() == "\\":
                return self._check_universal_char_name(offset=1) != 0
            return self._look_pred(0, lambda ch: ch.isalpha() or ch == "_")

        def _lex_ident(self) -> TokenKind:
            while True:
                if self._match_prefix("\\") and not self._lex_universal_char_name():
                    return TokenKind.ERROR
                if self._look_pred(0, lambda ch: ch.isalnum() or ch == "_"):
                    self._move()
                else:
                    return TokenKind.IDENT

        def _is_int_start(self) -> bool:
            return self._look_pred(0, lambda ch: ch.isdigit())

        def _lex_int_hex(self) -> bool:
            if not self._look_pred(0, _is_hex_digit):
                return False
            while True:
                self._move()
                if not self._look_pred(0, _is_hex_digit):
                    return True

        def _lex_int_octal(self) -> bool:
            while self._look_pred(0, _is_oct_digit):
                self._move()
            if self._look_pred(0, lambda ch: ch.isdigit()):
                # 8 or 9 in octal constant
                self._move()
                return False
            return True

        def _lex_int_decimal(self) -> bool:
            if not self._look_pred(0, lambda ch: ch.isdigit()):
                return False
            while True:
                self._move()
                if not self._look_pred(0, lambda ch: ch.isdigit()):
                    return True

        def _check_int_end(self) -> bool:
            return not (
                self._is_int_start()
                or self._is_char_start()
                or self._is_string_start()
                or self._is_ident_start()
            )

        def _lex_int_suffix(self) -> bool:
            # TODO(kyle): side effects are kinda problematic here.
            # the control flow is obstructed
            if (
                self._match_prefix("u")
                or self._match_prefix("U")
                and (
                    self._match_prefix("ll")
                    or self._match_prefix("LL")
                    or self._match_prefix("l")
                    or self._match_prefix("L")
                )
            ) or (
                self._match_prefix("ll")
                or self._match_prefix("LL")
                or self._match_prefix("l")
                or self._match_prefix("L")
                and (self._match_prefix("u") or self._match_prefix("U"))
            ):
                return self._check_int_end()
            return self._check_int_end()

        def _lex_int(self) -> TokenKind:
            if self._has_prefix("0x") or self._has_prefix("0X"):
                self._move(2)
                if not self._lex_int_hex():
                    return TokenKind.ERROR
                kind = TokenKind.C_HEX
            elif self._look() == "0":
                self._move()
                if not self._lex_int_octal():
                    return TokenKind.ERROR
                kind = TokenKind.C_OCTAL
            else:
                if not self._lex_int_decimal():
                    return TokenKind.ERROR
                kind = TokenKind.C_DECIMAL

            if not self._lex_int_suffix():
                return TokenKind.ERROR
            return kind

        def __iter__(self) -> "Lexer._Iterator":
            return self

        def __next__(self) -> Token:
            if self._was_eof:
                raise StopIteration
            self._skip_ws()

            kind = TokenKind.ERROR
            text: str | None = None
            start = self._pos

            if self._look() is None:
                self._was_eof = True
                kind = TokenKind.EOF
            elif self._is_punct():
                kind = self._lex_punct()
            elif self._is_string_start():
                kind = self._lex_string(start=start)
            elif self._is_char_start():
                kind = self._lex_char(start=start)
            elif self._is_ident_start():
                kind = self._lex_ident()
            elif self._is_int_start():
                kind = self._lex_int()
            else:
                self._move()

            if text is None:
                text = self._lex._src[start : self._pos]
            return Token(kind=kind, pos=start, text=text)
