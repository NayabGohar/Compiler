# LEXER VERSION B (NO REGEX)

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
}

OPERATORS = {
    "==": "T_EQUALSOP",
    "!=": "T_NOTEQUAL",
    "<=": "T_LEQ",
    ">=": "T_GEQ",
    "&&": "T_AND",
    "||": "T_OR",
    "=": "T_ASSIGNOP",
    "<": "T_LT",
    ">": "T_GT",
    "+": "T_PLUS",
    "-": "T_MINUS",
    "*": "T_MUL",
    "/": "T_DIV",
    "%": "T_MOD",
}

SYMBOLS = {
    "(": "T_PARENL",
    ")": "T_PARENR",
    "{": "T_BRACEL",
    "}": "T_BRACER",
    "[": "T_BRACKETL",
    "]": "T_BRACKETR",
    ";": "T_SEMICOLON",
    ",": "T_COMMA",
}

def lexer_manual(code: str):
    tokens = []
    i = 0
    n = len(code)

    def peek(offset=0):
        return code[i+offset] if i+offset < n else ""

    while i < n:
        ch = code[i]

        # Skip whitespace
        if ch.isspace():
            i += 1
            continue

        # Comments
        if ch == "/" and peek(1) == "/":
            while i < n and code[i] != "\n":
                i += 1
            continue
        if ch == "/" and peek(1) == "*":
            i += 2
            while i < n-1 and not (code[i] == "*" and code[i+1] == "/"):
                i += 1
            i += 2
            continue

        # String literal
        if ch == '"':
            start = i
            i += 1
            literal = ""
            while i < n and code[i] != '"':
                if code[i] == "\\" and i+1 < n:
                    literal += code[i] + code[i+1]  # keep escape sequences intact
                    i += 2
                else:
                    literal += code[i]
                    i += 1
            if i >= n:
                raise SyntaxError("Unterminated string literal")
            i += 1  # skip closing quote
            tokens.append(f"T_STRINGLIT(\"{literal}\")")
            continue

        # Numbers (int, float, scientific notation)
        if ch.isdigit():
            start = i
            has_dot = False
            has_exp = False
            i += 1

            while i < n:
                if code[i].isdigit():
                    i += 1
                elif code[i] == "." and not has_dot and not has_exp:
                    has_dot = True
                    i += 1
                elif code[i] in "eE" and not has_exp:
                    has_exp = True
                    i += 1
                    if i < n and code[i] in "+-":  # exponent sign
                        i += 1
                else:
                    break

            value = code[start:i]

            if has_dot or has_exp:
                tokens.append(f"T_FLOATLIT({value})")
            else:
                tokens.append(f"T_INTLIT({value})")
            continue

        # Identifiers / Keywords (allow Unicode and emojis)
        if ch.isalpha() or ch == "_" or ord(ch) > 127:
            start = i
            while i < n and (code[i].isalnum() or code[i] == "_" or ord(code[i]) > 127):
                i += 1
            value = code[start:i]

            if value in KEYWORDS:
                tokens.append(KEYWORDS[value])
            else:
                tokens.append(f"T_IDENTIFIER({value})")
            continue

        # Operators (multi-char first)
        matched = False
        for op in sorted(OPERATORS.keys(), key=len, reverse=True):
            if code.startswith(op, i):
                tokens.append(OPERATORS[op])
                i += len(op)
                matched = True
                break
        if matched:
            continue

        # Symbols
        if ch in SYMBOLS:
            tokens.append(SYMBOLS[ch])
            i += 1
            continue

        raise SyntaxError(f"Unexpected character: {ch} at position {i}")

    return tokens


# Example Usage

if __name__ == "__main__":
    code = """
    fn int my_fn(int x, float y) {
        string my_str = "hmm";
        bool my_bool = x == 40;
        float pi = 3.14;
        float big = 6.022e23;
        float small = 1.2E-3;
        return x;
    }
    """
    print("Manual Lexer Output:")
    print(lexer_manual(code))

    extra_code = """
    // comment line
    /* multi
       line
       comment */
    if (x != 10 && y >= 5) {
        my_var = "unicode: 😀😶";
        num = 2e10;
    }
    """
    print(lexer_manual(extra_code))

    # Invalid identifier test
    try:
        print(lexer_manual("int 123abc = 5;"))
    except SyntaxError as e:
        print("Caught error:", e)


    # 1. Mixed data types
    code1 = "int a = 10; float b = 5.25; string s = \"test123\";"
    print("Test 1:", lexer_manual(code1))

    # 2. Boolean logic & operators
    code2 = "bool flag = (a >= 10) || (b < 5) && (s != \"hi\");"
    print("Test 2:", lexer_manual(code2))

    # 3. Nested blocks & loops
    code3 = """
    fn int factorial(int n) {
        int result = 1;
        while (n > 1) {
            result = result * n;
            n = n - 1;
        }
        return result;
    }
    """
    print("Test 3:", lexer_manual(code3))

    # 4. Complex identifiers (Unicode + underscores)
    code4 = "var_😀😀😶 = 12345; 变量 = 42;"
    print("Test 4:", lexer_manual(code4))

    # 5. Edge cases: unterminated string, invalid char
    try:
        print("Test 5:", lexer_manual("string x = \"oops;"))
    except SyntaxError as e:
        print("Test 5 Caught error:", e)

    try:
        print("Test 6:", lexer_manual("int y = 99 @ 5;"))
    except SyntaxError as e:
        print("Test 6 Caught error:", e)

