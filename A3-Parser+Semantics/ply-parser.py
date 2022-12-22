import ply.lex as lex
import ply.yacc as yacc
import os

##########################################################
#                         SCANNER
##########################################################

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


lexer = lex.lex()



##########################################################
#                         PARSER
##########################################################

def p_defdefs(p):
    ''' defdefs : defdef defdefs
                | defdef '''
    # Semantics
    if len(p) == 3:
        p[0] = [p[1]], p[2]
    elif len(p) == 2:
        p[0] = [p[1]]


def p_defdef(p):
    ''' defdef : DEF ID LPAREN parmsopt RPAREN COLON type BECOMES LBRACE vardefsopt defdefsopt expras RBRACE '''
    # Semantics
    [p[7]] = [p[12]]


def p_parmsopt(p):
    ''' parmsopt : parms
                 | empty '''
    # Semantics
    p[0] = [p[1]]
    

def p_parms(p):
    ''' parms : vardef COMMA parms
              | vardef '''
    # Semantics
    if len(p) == 4:
        p[0] = [p[1]] , p[3]
    elif len(p) == 2:
        p[0] = [p[1]]


def p_vardef(p):
    ''' vardef : ID COLON type '''
    # Semantics
    p[0] = [p[3]]


def p_type(p):
    ''' type : INT
             | LPAREN typesopt RPAREN ARROW type '''
    # Semantics
    if len(p) == 2:
        p[0] = p[1]
    elif len(p) == 6:
        p[0] = [p[2]] , p[5]


def p_typesopt(p):
    ''' typesopt : types
                 | empty '''
    # Semantics
    p[0] = p[1]


def p_types(p):
    ''' types : type COMMA types
              | type '''
    # Semantics
    if len(p) == 4:
        p[0] = [p[1]] , p[3]
    elif len(p) == 2:
        p[0] = [p[1]]


def p_vardefsopt(p):
    ''' vardefsopt : VAR vardef SEMI vardefsopt
                   | empty '''
    # Semantics
    if len(p) == 5:
        p[0] = [p[2]] , p[4]
    elif len(p) == 2:
        p[0] = p[1]


def p_defdefsopt(p):
    ''' defdefsopt : defdefs
                   | empty '''
    # Semantics
    p[0] = [p[1]]


def p_expras(p):
    ''' expras : expra SEMI expras
               | expra '''
    # Semantics
    if len(p) == 4:
        p[0] = p[3]
    elif len(p) == 2:
        p[0] = [p[1]]


def p_expra(p):
    ''' expra : ID BECOMES expr
              | expr '''
    # Semantics
    if len(p) == 4:
        p[0] = [p[3]] = p[1]
    elif len(p) == 2:
        p[0] = [p[1]]


def p_expr(p):
    ''' expr : IF LPAREN test RPAREN LBRACE expras RBRACE ELSE LBRACE expras RBRACE
             | term
             | expr PLUS term
             | expr MINUS term '''
    # Semantics
    if len(p) == 2:
        p[0] = [p[1]]
    elif len(p) == 4:
        p[3] = 'Int'
        p[0] = p[1] = [p[3]]
    elif len(p) == 12:
        [p[3]] = 'Int'
        p[0] = [p[6]] = [p[10]]


def p_term(p):
    ''' term : factor
             | term STAR factor
             | term SLASH factor
             | term PCT factor '''
    # Semantics
    if len(p) == 2:
        p[0] = [p[1]]
    elif len(p) == 4:
        p[0] = p[1] = [p[3]] = 'Int'


def p_factor(p):
    ''' factor : ID
               | NUM
               | LPAREN expr RPAREN
               | factor LPAREN argsopt RPAREN '''
    # Semantics
    ID = r'[a-zA-Z_][a-zA-Z_0-9]*'
    NUM = r'\d+'
    if len(p) == 2:
        if p[1] == ID:
            p[0] = p[1]
        if p[1] == NUM:
            p[0] = 'Int'
    elif len(p) == 4:
        p[0] = [p[2]]
    elif len(p) == 5:
        p[0] = p[1] = [p[3]]
    

def p_test(p):
    ''' test : expr NE expr
             | expr LT expr
             | expr LE expr
             | expr GE expr
             | expr GT expr
             | expr EQ expr '''
    # Semantics
    p[0] = [p[1]] , [p[3]]


def p_argsopt(p):
    ''' argsopt : args
                | empty '''
    # Semantics
    p[0] = [p[1]]


def p_args(p):
    ''' args : expr COMMA args
             | expr '''
    # Semantics
    if len(p) == 4:
        p[0] = [p[1]] , p[3]
    elif len(p) == 2:
        p[0] = [p[1]]


def p_error(p):
    print(f"Syntax error at token: {p.type} ('{p.value}'), line {p.lineno}")
    parser.errok()


def p_empty(p):
    ''' empty : '''
    # Semantics
    p[0] = None


parser = yacc.yacc()


# Gets working directory and saves tests' location
directory = os.getcwd()
test1 = os.path.join(directory, "TestProgram1.txt")
test2 = os.path.join(directory, "TestProgram2.txt")

# Goes through the contents of both test files provided
with open(test1) as t1, open(test2,) as t2:
    while True:
        try:
            data1 = t1.read()
            data2 = t2.read()
        except EOFError:
            break
        if not data1 or not data2:
            continue
        # Runs the parser for first test file
        print("Parsing TestProgram1 ...")
        parser.parse(data1)
        print("Done!")

        # Runs the parser for second test file
        print("\nParsing TestProgram2 ...")
        parser.parse(data2)
        print("Done!")
        break
