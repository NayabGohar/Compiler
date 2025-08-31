# With Uni-code
#Lexer Version A (With Regex)
import re
from typing import List

# ---------------------------------------------
# - Avoids unsupported \p{..} escapes
# - Handles comments, strings with escapes, Unicode & emoji identifiers
# - Emits separate T_QUOTES around string literals
# - Throws on invalid identifiers starting with a digit
# ---------------------------------------------

# Keywords
KEYWORDS = {
    "fn": "T_FUNCTION",
    "int": "T_INT",
    "float": "T_FLOAT",
    "string": "T_STRING",
    "bool": "T_BOOL",
    "return": "T_RETURN",
    "if": "T_IF",
    "else": "T_ELSE",
    "while": "T_WHILE",
    "for": "T_FOR",
    # extras
    "char": "T_CHAR",
    "double": "T_DOUBLE",
    "void": "T_VOID",
    "true": "T_TRUE",
    "false": "T_FALSE",
}

# Multi-character operators (longest first)
MULTI_OPS = [
    ("==", "T_EQUALSOP"),
    ("!=", "T_NOTEQUAL"),
    ("<=", "T_LEQ"),
    (">=", "T_GEQ"),
    ("&&", "T_AND"),
    ("||", "T_OR"),
    ("<<", "T_LSHIFT"),
    (">>", "T_RSHIFT"),
]

# Single-character operators / punctuators
SINGLE_OPS = {
    "=": "T_ASSIGNOP",
    "<": "T_LT",
    ">": "T_GT",
    "+": "T_PLUS",
    "-": "T_MINUS",
    "*": "T_MUL",
    "/": "T_DIV",
    "%": "T_MOD",
    "&": "T_BITAND",
    "|": "T_BITOR",
    "^": "T_BITXOR",
    "~": "T_BITNOT",
    "(": "T_PARENL",
    ")": "T_PARENR",
    "{": "T_BRACEL",
    "}": "T_BRACER",
    "[": "T_BRACKETL",
    "]": "T_BRACKETR",
    ";": "T_SEMICOLON",
    ",": "T_COMMA",
    '"': "T_QUOTES",
}

# Regexes

RE_WS = re.compile(r"\s+", re.UNICODE)
RE_INT = re.compile(r"\d+", re.UNICODE)

# Identifier with Unicode + emoji support:
# start: letter/underscore OR emoji range U+1F300–U+1FAFF
# rest:  \w (letters/digits/_) OR emoji range

EMOJI_RANGE = "\U0001F300-\U0001FAFF"
RE_IDENT = re.compile(rf"(?:[^\W\d]|_|[{EMOJI_RANGE}])(?:\w|[{EMOJI_RANGE}])*", re.UNICODE)

class LexError(SyntaxError):
    pass

class Lexer:
    def __init__(self, code: str):
        self.code = code
        self.n = len(code)
        self.i = 0
        self.line = 1
        self.col = 1
        self.tokens: List[str] = []

    # --- utility ---
    def _advance(self, text: str):
        lines = text.split("\n")
        if len(lines) == 1:
            self.col += len(text)
        else:
            self.line += len(lines) - 1
            self.col = len(lines[-1]) + 1
        self.i += len(text)

    def _peek(self, k: int = 0) -> str:
        j = self.i + k
        return self.code[j] if 0 <= j < self.n else ""

    def _match_regex(self, regex: re.Pattern) -> str | None:
        m = regex.match(self.code, self.i)
        if m:
            return m.group(0)
        return None

    # --- scanners ---
    
    def _scan_whitespace(self) -> bool:
        m = self._match_regex(RE_WS)
        if m:
            self._advance(m)
            return True
        return False

    def _scan_comment(self) -> bool:
        if self._peek() == "/" and self._peek(1) in ("/", "*"):
            if self._peek(1) == "/":  # //...
                j = self.i + 2
                while j < self.n and self.code[j] != "\n":
                    j += 1
                text = self.code[self.i:j]
                self.tokens.append(f'T_COMMENT({text})')
                self._advance(text)
                return True
            else:  # /* ... */
                j = self.i + 2
                while j < self.n - 1 and not (self.code[j] == "*" and self.code[j+1] == "/"):
                    j += 1
                if j >= self.n - 1:
                    raise LexError(f"Unterminated comment at line {self.line}, col {self.col}")
                text = self.code[self.i:j+2]
                self.tokens.append(f'T_COMMENT({text})')
                self._advance(text)
                return True
        return False

    def _scan_string(self) -> bool:
        if self._peek() == '"':
            # opening quote
            self.tokens.append("T_QUOTES")
            self._advance('"')
            buf = []
            while True:
                if self.i >= self.n:
                    raise LexError(f"Unterminated string at line {self.line}, col {self.col}")
                ch = self._peek()
                if ch == "\\":  # escape
                    if self.i + 1 >= self.n:
                        raise LexError(f"Bad escape at EOF (line {self.line}, col {self.col})")
                    nxt = self._peek(1)
                    buf.append("\\" + nxt)  # keep escapes verbatim in payload
                    self._advance("\\" + nxt)
                    continue
                if ch == '"':
                    lit = ''.join(buf)
                    self.tokens.append(f'T_STRINGLIT("{lit}")')
                    self.tokens.append("T_QUOTES")
                    self._advance('"')
                    return True
                buf.append(ch)
                self._advance(ch)
        return False

    def _scan_number_or_invalid(self) -> bool:
        m = self._match_regex(RE_INT)
        if not m:
            return False

        # If digits are followed by id-start, it's an invalid identifier like 123abc

        nxt = self._peek(len(m))
        if nxt and (nxt == '_' or nxt.isalpha()):
            j = self.i + len(m)
            while j < self.n and (self.code[j].isalnum() or self.code[j] == '_' or RE_IDENT.match(self.code[j])):
                j += 1
            bad = self.code[self.i:j]
            raise LexError(f"Invalid identifier starting with number: '{bad}' at line {self.line}, col {self.col}")
        self.tokens.append(f'T_INTLIT({m})')
        self._advance(m)
        return True

    def _scan_identifier_or_keyword(self) -> bool:
        m = self._match_regex(RE_IDENT)
        if not m:
            return False
        if m[0].isdigit():
            raise LexError(f"Invalid identifier: '{m}' at line {self.line}, col {self.col}")
        kind = KEYWORDS.get(m)
        if kind:
            self.tokens.append(kind)
        else:
            self.tokens.append(f'T_IDENTIFIER("{m}")')
        self._advance(m)
        return True

    def _scan_operator_or_punct(self) -> bool:
        for op, name in MULTI_OPS:
            if self.code.startswith(op, self.i):
                self.tokens.append(name)
                self._advance(op)
                return True
        ch = self._peek()
        if ch in SINGLE_OPS:
            self.tokens.append(SINGLE_OPS[ch])
            self._advance(ch)
            return True
        return False

    def tokenize(self) -> List[str]:
        while self.i < self.n:
            if self._scan_whitespace():
                continue
            if self._scan_comment():
                continue
            if self._scan_string():
                continue
            if self._scan_number_or_invalid():
                continue
            if self._scan_identifier_or_keyword():
                continue
            if self._scan_operator_or_punct():
                continue
            ch = self._peek()
            raise LexError(f"Unexpected character '{ch}' at line {self.line}, col {self.col}")
        return self.tokens


def lexer(code: str) -> List[str]:
    return Lexer(code).tokenize()



# Example Usage & Tests


if __name__ == "__main__":
    # Given sample
    code = (
        'fn int my_fn(int x, float y) {\n'
        '    string my_str = "hmm";\n'
        '    bool my_bool = x == 40;\n'
        '    return x;\n'
        '}\n'
    )
    print("TEST 1 — sample program:")
    print(lexer(code))
    print()

    # Comments, unicode in strings, logical ops
    extra_code = (
        '// comment line\n'
        '/* multi\n   line\n   comment */\n'
        'if (x != 10 && y >= 5) {\n'
        '    my_var = "unicode: 😀 and escape: \"ok\"\n\tline";\n'
        '}\n'
    )
    print("TEST 2 — comments, escapes, unicode:")
    print(lexer(extra_code))
    print()

    # Bitwise ops, shifts, arrays
    bitwise_code = (
        'int main() {\n'
        '  a = b & c | d ^ ~e;\n'
        '  arr[2] = x << 1;\n'
        '  y = z >> 3;\n'
        '}\n'
    )
    print("TEST 3 — bitwise & shifts:")
    print(lexer(bitwise_code))
    print()

    # Unicode identifiers (including emoji), plus return
    unicode_id_code = (
        'fn int 😶😀😶sum(int μ, int 树) {\n'
        '  int 变量 = μ + 树;\n'
        '  return 变量;\n'
        '}\n'
    )
    print("TEST 4 — unicode & emoji identifiers:")
    print(lexer(unicode_id_code))
    print()

    # Invalid identifier (should raise)
    invalid_code = 'int 2bad = 3;\n'
    print("TEST 5 — invalid identifier (expect error):")
    try:
        print(lexer(invalid_code))
    except LexError as e:
        print("Caught:", e)
