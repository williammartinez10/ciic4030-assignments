import ply.lex as lex

# Reserved keywords
reserved = {
    "def" : "DEF",
    "var" : "VAR",
    "Int" : "INT",
    "if" : "IF",
    "else" : "ELSE"
}

# Token names
tokens = (
    'ID',
    'NUM',
    'LPAREN',
    'RPAREN',
    'LBRACE',
    'RBRACE',
    'BECOMES',
    'EQ',
    'NE',
    'LT',
    'GT',
    'LE',
    'GE',
    'PLUS',
    'MINUS',
    'STAR',
    'SLASH',
    'PCT',
    'COMMA',
    'SEMI',
    'COLON',
    'ARROW',
    'COMMENT',
    'WHITESPACE'
) + tuple(reserved.values())


# Regular expression rules for simple tokens
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_LBRACE = r'\{'
t_RBRACE = r'\}'
t_BECOMES = r'\='
t_EQ = r'\=='
t_NE = r'\!='
t_LT = r'\<'
t_GT = r'\>'
t_LE = r'\<='
t_GE = r'\>='
t_PLUS = r'\+'
t_MINUS = r'-'
t_STAR = r'\*'
t_SLASH = r'/'
t_PCT = r'\%'
t_COMMA = r'\,'
t_SEMI = r'\;'
t_COLON = r'\:'
t_ARROW = r'\=>'


# Regular expression with action code for validation of IDs
def t_ID(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = reserved.get(t.value, 'ID')
    return t

# Regular expression with action code for validation of numbers
def t_NUM(t):
    r'\d+'
    t.value = int(t.value)
    return t

# Regular expression with action code for validation of comments
# and removes (ignores) them from the sequence of tokens.
def t_COMMENT(t):
    r'\//.*'
    t.type = reserved.get(t.value, 'COMMENT')
    pass

# Regular expression with action code for validation of whitespaces
# and removes (ignores) them from the sequence of tokens.
def t_WHITESPACE(t):
    r'\ [\t-\n-\r]?'
    t.type = reserved.get(t.value, 'WHITESPACE')
    pass

# Tracks line numbers
def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

# Handles errors
def t_error(t):
    print("Invalid token '%s'" % t.value[0])
    t.lexer.skip(1)


# Tests/runs the lexer
lexer = lex.lex()

data = '''\
            def f(a:Int, b:Int):Int = { var c:Int;
            def g(a:Int, b:(Int)=>Int):Int = { b(a)
            } def h(c:Int):Int = {
            def g():Int = { c-b
            }
            g() }
            c = a+b;
            g(c,h) }
       '''

# Alternate way for reading an external file, instead of storing the contents of a file inside a string
# with open("Test_program.rtf") as f:
#     data = f.read()

lexer.input(data)
while True:
    t = lexer.token()
    if not t:
        break
    print(t)
