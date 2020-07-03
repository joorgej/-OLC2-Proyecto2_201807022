import ply.lex as lex
import ply.yacc as yacc
import re
from tablaSimbolos import *
from instruction import *
from traductor import *
from functions import *

'''/////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////        FUNCIONES         /////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////'''

functions = {}
actualFunction = ''
callIndex = -1
functionCallIndex = 0

def addFunction(function):
    global functions
    functions[function.name] = function
    global actualFunction
    newFunctionCallIndex()
    actualFunction = function.name

def getReturn():
    global functions
    global actualFunction
    return functions[actualFunction].returns

def getReturnName(name):
    global functions
    return functions[name].returns

def addCall(name, call):
    global functions
    functions[name].newCall(call)

def getCallName(name):
    global functions
    return name+'_call_'+str(len(functions[name].calls))

def getCallIndex():
    global callIndex
    callIndex += 1
    return callIndex

def getCallReturn(name):
    global functions
    return functions[name].return_call

def getFunctionCallIndex(name):
    global functions
    return functions[name].call_index

def newFunctionCallIndex():
    global functionCallIndex
    functionCallIndex += 1
    global callIndex
    callIndex = -1

def getFunctionParams(name):
    global functions
    return functions[name].parameters

'''/////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////        ERRORS         /////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////'''

errores = ''

def newError(error):
    global errores
    errores = errores + error



'''/////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////        GRAMMAR         ////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////'''

gramatica = ''

def addProduction(prod):
    global gramatica
    gramatica = prod + gramatica

'''/////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////        INDICES         ////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////'''

tempIndex = -1
paramIndex = -1
retIndex = -1
ifIndex = -1
labelIndex = 0
forIndex = -1
whileIndex = -1
doIndex = -1

def getTemp():
    global tempIndex
    tempIndex += 1
    return '$t'+str(tempIndex)

def getParam():
    global paramIndex
    paramIndex += 1
    return '$a'+str(paramIndex)
    
def getRet():
    global retIndex
    retIndex += 1
    return '$s'+str(retIndex)

def getIf():
    global ifIndex
    ifIndex += 1
    return 'if'+str(ifIndex)
    
def getWhile():
    global whileIndex
    whileIndex += 1
    return 'while'+str(whileIndex)

def getDo():
    global doIndex
    doIndex += 1
    return 'do'+str(doIndex)

def getFor():
    global forIndex
    forIndex += 1
    return 'for'+str(forIndex)


'''/////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////        INSTRUCCIONES         ///////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////'''

labels = [Label('main',[Set('$ra', '', 'array()', None)])]
ifLabels = []

def addIfLabel():
    global ifLabels
    ifLabels = ifLabels + [len(labels)-1]

def deleteIfLabel():
    global ifLabels
    ifLabels = []

def addInstruction(instruc):
    global labels
    labels[len(labels)-1].instructions = labels[len(labels)-1].instructions+[instruc]

def addInstructionMain(instruc):
    global labels
    labels[0].instructions = labels[0].instructions+[instruc]

def addLabel(label):
    global labels
    labels = labels + [label]
    



'''/////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////        TABLA SIMBOLOS         /////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////'''

ts = TablaSimbolos()
scope = [[]]
actualIndex = 0

def newScope():
    global actualIndex
    actualIndex += 1
    global scope
    scope = scope + [[]]

def deleteScope():
    global actualIndex
    global scope
    if len(scope)>0:
        for key in scope[actualIndex]:
            ts.delete(key)
        scope.pop(actualIndex)
        actualIndex -= 1

def addScope(key, name, val, dim = None):
    scope[actualIndex] = scope[actualIndex]+[key]
    ts.add(key, name, val, dim)

def addGlobal(key, name, val):
    scope[0] = scope[0]+[key]
    ts.add(key, name, val)

'''/////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////        TOKENS         /////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////'''


reservadas = {
    'int' : 'INT',
    'double' : 'DOUBLE',
    'char' : 'CHAR',
    'goto' : 'GOTO',
    'main' : 'MAIN',
    'if' : 'IF',
    'else' : 'ELSE',
    'void' : 'VOID',
    'while' : 'WHILE',
    'break' : 'BREAK',
    'continue' : 'CONTINUE',
    'do' : 'DO',
    'for' : 'FOR',
    'struct' : 'STRUCT',
    'switch' : 'SWITCH',
    'case' : 'CASE',
    'default' : 'DEFAULT',
    'return' : 'RETURN'
}


tokens = (
    'ES_IGUAL',
    'NO_IGUAL',
    'SUMA_IGUAL',
    'RESTA_IGUAL',
    'MULT_IGUAL',
    'DIV_IGUAL',
    'MOD_IGUAL',
    'SHIFT_IZQ_IGUAL',
    'SHIFT_DER_IGUAL',
    'AND_BIT_IGUAL',
    'OR_BIT_IGUAL',
    'XOR_BIT_IGUAL',
    'MAYOR_IGUAL',
    'MENOR_IGUAL',
    'INTERROGACION',
    'SHIFT_DER',
    'SHIFT_IZQ',
    'MAYOR',
    'MENOR',
    'XOR_BIT',
    'OR',
    'AND',
    'NOT_BIT',
    'AND_BIT',
    'OR_BIT',
    'NOT',
    'SUMA',
    'SUMA_SUMA',
    'RESTA',
    'RESTA_RESTA',
    'MULT',
    'DIV',
    'MOD',
    'PAR_IZQ',
    'PAR_DER',
    'COR_IZQ',
    'COR_DER',
    'BRA_IZQ',
    'BRA_DER',
    'IGUAL',
    'DOS_PUNTOS',
    'PUNTO_COMA',
    'COMA',
    'PUNTO',
    'INT',
    'DOUBLE',
    'CHAR',
    'GOTO',
    'MAIN',
    'ID',
    'COMENTARIO',
    'COMENTARIO_MULTILINEA',
    'ENTERO',
    'DECIMAL',
    'CARACTER',
    'CADENA',
    'VOID',
    'WHILE',
    'BREAK',
    'CONTINUE',
    'DO',
    'FOR',
    'STRUCT',
    'SWITCH',
    'CASE',
    'DEFAULT',
    'IF',
    'ELSE',
    'RETURN'
)

'''/////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////        ANALISIS LEXICO         /////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////'''

t_ignore = ' \t'
t_ES_IGUAL = r'[=][=]'
t_NO_IGUAL = r'[!][=]'
t_SUMA_IGUAL = r'[+][=]'
t_RESTA_IGUAL = r'[-][=]'
t_MULT_IGUAL = r'[*][=]'
t_DIV_IGUAL = r'[/][=]'
t_MOD_IGUAL = r'[%][=]'
t_SHIFT_IZQ_IGUAL = r'[<][<][=]'
t_SHIFT_DER_IGUAL = r'[>][>][=]'
t_AND_BIT_IGUAL = r'[&][=]'
t_OR_BIT_IGUAL = r'[|][=]'
t_XOR_BIT_IGUAL = r'[\\^][=]'
t_MAYOR_IGUAL = r'[>][=]'
t_MENOR_IGUAL = r'[<][=]'
t_INTERROGACION = r'[?]'
t_SHIFT_DER = r'[>][>]'
t_SHIFT_IZQ = r'[<][<]'
t_MAYOR = r'[>]'
t_MENOR = r'[<]'
t_XOR_BIT = r'[\^]'
t_OR = r'[|][|]'
t_AND = r'[&][&]'
t_NOT_BIT = r'[~]'
t_AND_BIT = r'[&]'
t_OR_BIT = r'[|]'
t_NOT = r'[!]'
t_SUMA = r'[+]'
t_SUMA_SUMA = r'[+][+]'
t_RESTA = r'[-]'
t_RESTA_RESTA = r'[-][-]'
t_MULT = r'[*]'
t_DIV = r'[/]'
t_MOD = r'[%]'
t_PAR_IZQ = r'[(]'
t_PAR_DER = r'[)]'
t_COR_IZQ = r'[[]'
t_COR_DER = r'[]]'
t_BRA_IZQ = r'[{]'
t_BRA_DER = r'[}]'
t_IGUAL = r'[=]'
t_DOS_PUNTOS = r'[:]'
t_PUNTO_COMA = r'[;]'
t_COMA = r'[,]'
t_PUNTO = r'[.]'


def t_CARACTER(t):
    r"['].[']"
    return t

def t_CADENA(t):
    r'\"([^\"]|\\.")*\"'
    return t

def t_ID(t):
    r'[a-zA-Z][a-zA-Z0-9_]*'
    t.type = reservadas.get(t.value,'ID')
    return t

def t_DECIMAL(t):
    r'\d+\.\d+'
    return t

def t_ENTERO(t):
    r'\d+'
    return t

def t_COMENTARIO(t):
    r'//.*\n'
    t.lexer.lineno += 1

def t_COMENTARIO_MULTILINEA(t):
    r'/\*(.|\n)*\*/'
    t.lexer.lineno += t.value.count("\n")

def t_NEWLINE(t):
    r'\n+'
    t.lexer.lineno += t.value.count("\n")
    
def t_error(t):
    print(t.value)
    print('ERROR LEXICO:   El simbolo "' + t.value[0] + '", declarado en la linea ' + str(t.lineno) + ', no pertenece al lenguaje. \n')
    t.lexer.skip(1)


'''/////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////        PRECEDENCIA         /////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////'''

precedence =(
    ('left', 'OR'),
    ('left', 'AND'),
    ('left', 'OR_BIT'),
    ('left', 'XOR_BIT'),
    ('left', 'AND_BIT'),
    ('nonassoc', 'ES_IGUAL', 'NO_IGUAL'),
    ('nonassoc', 'MAYOR', 'MENOR', 'MAYOR_IGUAL', 'MENOR_IGUAL'),
    ('left', 'SHIFT_DER', 'SHIFT_IZQ'),
    ('left', 'SUMA', 'RESTA'),
    ('left', 'MULT', 'DIV', 'MOD'),
    ('right', 'MENOS', 'NOT', 'NOT_BIT', 'REFERENCIA', 'PRE_INC', 'PRE_DEC'),
    ('left', 'POST_INC', 'POST_DEC')
)

'''/////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////        ANALISIS SINTACTICO         ////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////'''

def p_init(t):
    'init                       : program'
    addProduction('init → program')
    global functions
    cont = 0
    for func in functions:
        if len(functions[func].calls) != 0:
            addLabel(Label(functions[func].return_call, []))
            for call in functions[func].calls:
                addInstruction(If('$ra['+str(cont)+'] == '+str(call.index), call.label))
            cont += 1
        


    
def p_init_empty(t):
    'init                       : '
    addProduction('init → ε')

def p_program_main(t):
    'program                    : main'
    addProduction('program → main')

def p_program_global_declar_main(t):
    'program                    : declars main'
    addProduction('program → declars main')

def p_declars(t):
    'declars                    : declars declar'
    addProduction('declars → declars declar')

def p_declars_end(t):
    'declars                    : declar'
    addProduction('declars → declar')
   
def p_declar_func(t):
    'declar                     : primitive_type function'
    addProduction('declar → primitive_type function')

def p_fuction_id(t):
    'declar                     : VOID function'
    addProduction('declar → VOID function')

def p_global_declaration(t):
    'declar                     : declaration PUNTO_COMA'
    addProduction('declar → declaration')

def p_global_assignation(t):
    'declar                     : assignation PUNTO_COMA'
    addProduction('declar → assignation')

def p_declar_struc(t):
    'declar                     : struct'
    addProduction('declar → struct')
    
def p_main(t):
    'main                       : INT MAIN new_label PAR_IZQ PAR_DER function_block'
    addProduction('main → MAIN PAR_IZQ PAR_DER function_block')
    addInstruction(Exit())

def p_new_label(t):
    'new_label                  : '

    if t[-1]=='main':
        addInstructionMain(Goto('_main_'))
        addLabel(Label('_main_', []))
    else:
        addLabel(Label(t[-1], []))

    t[0] = 'nep'

def p_function(t):
    'function                   : ID new_label param add_function function_block'
    addProduction('function → function_type ID param function_block')
    addInstruction(Goto(getCallReturn(t[1])))
    global actualFunction
    actualFunction = None

    
def p_add_function(t):
    'add_function               : '
    ret = None
    if t[-4] != 'void':
        ret = getRet()
    global functionCallIndex
    addFunction(Funcion(t[-3],t[-1],functionCallIndex, [], ret))

    for param in t[-1]:
        addInstructionMain(Set(param.name, '', '0', None))
        addGlobal(param.index, param.name, 0)
    
def p_param_empty(t):
    'param                      : PAR_IZQ PAR_DER'
    addProduction('param → PAR_IZQ PAR_DER')
    t[0] = []
    
def p_param(t):
    'param                      : PAR_IZQ parameter_list PAR_DER'
    addProduction('param → PAR_IZQ parameter_list PAR_DER')
    t[0] = t[2]
    
def p_param_list(t):
    'parameter_list             : parameter_list COMA parameter'
    addProduction('parameter_list → parameter_list COMA parameter')
    t[1].append(t[3])
    t[0] = t[1]
    
def p_param_list_end(t):
    'parameter_list             : parameter'
    addProduction('parameter_list → parameter')
    t[0] = [t[1]]
    
def p_parameter(t):
    'parameter                  : primitive_type ID'
    addProduction('parameter → primitive_type ID')
    t[0] = Parametro(t[2], getParam())
    
def p_primitive_type_int(t):
    'primitive_type             : INT'
    addProduction('primitive_type → INT')
    t[0] = t[1]

def p_primitive_type_float(t):
    'primitive_type             : DOUBLE'
    addProduction('primitive_type → DOUBLE')
    t[0] = t[1]

def p_primitive_type_char(t):
    'primitive_type             : CHAR'
    addProduction('primitive_type → CHAR')
    t[0] = t[1]

def p_primitive_type_struct(t):
    'primitive_type             : STRUCT ID'
    addProduction('primitive_type → STRUCT ID')
    t[0] = t[2]
    
def p_struct(t):
    'struct                     : STRUCT ID struct_block PUNTO_COMA'
    addProduction('struct → STRUCT ID struct_block')
    
def p_struct_block(t):
    'struct_block               : BRA_IZQ new_scope declarations BRA_DER'
    addProduction('struct_block → BRA_IZQ declarations BRA_DER')
    deleteScope()

def p_function_block(t):
    'function_block             : BRA_IZQ new_scope instructions BRA_DER'
    addProduction('function_block → BRA_IZQ instructions BRA_DER')
    deleteScope()

def p_new_scope(t):
    'new_scope                  : '
    newScope()

def p_function_block_empty(t):
    'function_block             : BRA_IZQ BRA_DER'
    addProduction('function_block → BRA_IZQ BRA_DER')
    
def p_instructions(t):
    'instructions               : instructions instruction'
    addProduction('instructions → instructions instruction')
    
def p_instructions_end(t):
    'instructions               : instruction'
    addProduction('instructions → instruction')
    
def p_instruction_declaration(t):
    'instruction                : declaration PUNTO_COMA'
    addProduction('instruction → declaration PUNTO_COMA')
    
def p_instruction_assignation(t):
    'instruction                : assignation PUNTO_COMA'
    addProduction('instruction → assignation PUNTO_COMA')
    
def p_instruction_for(t):
    'instruction                : for'
    addProduction('instruction → for')
    
def p_instruction_while(t):
    'instruction                : while'
    addProduction('instruction → while')
    
def p_instruction_do(t):
    'instruction                : do PUNTO_COMA'
    addProduction('instruction → do PUNTO_COMA')
    
def p_instruction_if(t):
    'instruction                : if'
    addProduction('instruction → if')
    
def p_instruction_switch(t):
    'instruction                : switch'
    addProduction('instruction → switch')
    
def p_instruction_label(t):
    'instruction                : label'
    addProduction('instruction → label')
    
def p_instruction_goto(t):
    'instruction                : goto PUNTO_COMA'
    addProduction('instruction → goto PUNTO_COMA')
    
def p_instruction_break(t):
    'instruction                : break PUNTO_COMA'
    addProduction('instruction → break PUNTO_COMA')
    
def p_instruction_continue(t):
    'instruction                : continue PUNTO_COMA'
    addProduction('instruction → continue PUNTO_COMA')
    
def p_instruction_return(t):
    'instruction                : return PUNTO_COMA'
    addProduction('instruction → return PUNTO_COMA')
    
def p_instruction_expression(t):
    'instruction                : expression PUNTO_COMA'
    addProduction('instruction → expression PUNTO_COMA')
    
def p_instruction_null(t):
    'instruction                : PUNTO_COMA'
    addProduction('instruction → PUNTO_COMA')
    
def p_declarations(t):
    'declarations               : declarations declaration PUNTO_COMA'
    addProduction('declarations → declarations declaration PUNTO_COMA')
    
def p_declarations_end(t):
    'declarations               : declaration PUNTO_COMA'
    addProduction('declarations → declaration PUNTO_COMA')
    
def p_declaration(t):
    'declaration                : primitive_type declaration_list'
    addProduction('declaration → primitive_type declaration_list')
    
def p_declaration_struct(t):
    'declaration                : ID declaration_list'
    addProduction('declaration → ID declaration_list')
    
def p_declaration_list_val(t):
    'declaration_list           : declaration_list COMA ID IGUAL expression'
    addProduction('declaration_list → declaration_list COMA ID IGUAL expression')
    t[0] = getTemp()
    addScope(t[3], t[0], t[5])
    addInstruction(Set(t[0], '', t[5], None))
    
def p_declaration_list_arr(t):
    'declaration_list           : declaration_list COMA ID brackets IGUAL BRA_IZQ expression_list BRA_DER'
    addProduction('declaration_list → declaration_list COMA ID brackets IGUAL BRA_IZQ expression_list BRA_DER')
    t[0] = getTemp()
    addScope(t[3], t[0], t[7])
    addInstruction(Set(t[0], '', 'array()', None))

    contador = 0
    for exp in t[7]:
        addInstruction(Set(t[0]+'['+contador+']', '', exp, None))
        contador += 1

def p_declaration_list_id(t):
    'declaration_list           : declaration_list COMA ID'
    addProduction('declaration_list → declaration_list COMA ID')
    t[0] = getTemp()
    addScope(t[3], t[0], None)
    addInstruction(Set(t[0], '', 0, None))

def p_declaration_list_id_bracket(t):
    'declaration_list           : declaration_list COMA ID brackets'
    addProduction('declaration_list → declaration_list COMA ID brackets') 
    t[0] = getTemp()
    addScope(t[3], t[0], None)
    addInstruction(Set(t[0], '', 'array()', None))

def p_declaration_string_declaration(t):
    'declaration_list           : declaration_list COMA ID COR_IZQ COR_DER IGUAL CADENA'
    addProduction('declaration_list → declaration_list COMA ID COR_IZQ COR_DER IGUAL CADENA')
    t[0] = getTemp()
    addScope(t[1], t[0], t[5])
    addInstruction(Set(t[0], '', t[5], None))
    
def p_declaration_list_val_end(t):
    'declaration_list           : ID IGUAL expression'
    addProduction('declaration_list → ID IGUAL expression')
    t[0] = getTemp()
    addScope(t[1], t[0], t[3])
    addInstruction(Set(t[0], '', t[3], None))
    
def p_declaration_list_arr_end(t):
    'declaration_list           : ID brackets IGUAL BRA_IZQ expression_list BRA_DER'
    addProduction('declaration_list → ID brackets IGUAL BRA_IZQ expression_list BRA_DER')
    t[0] = getTemp()
    addScope(t[1], t[0], t[5])
    addInstruction(Set(t[0], '', 'array()', None))

    contador = 0
    for exp in t[5]:
        addInstruction(Set(t[0]+'['+str(contador)+']', '', exp, None))
        contador += 1
    
def p_declaration_list_id_end(t):
    'declaration_list           : ID'
    addProduction('declaration_list → ID')
    t[0] = getTemp()
    addScope(t[1], t[0], None)
    addInstruction(Set(t[0], '', 0, None))

def p_declaration_list_id_bracket_end(t):
    'declaration_list           : ID brackets'
    addProduction('declaration_list → ID brackets')
    t[0] = getTemp()
    addScope(t[1], t[0], None)
    addInstruction(Set(t[0], '', 'array()', None))
    
def p_declaration_string_declaration_end(t):
    'declaration_list           : ID COR_IZQ COR_DER IGUAL CADENA'
    addProduction('declaration_list → ID COR_IZQ COR_DER IGUAL CADENA')
    t[0] = getTemp()
    addScope(t[1], t[0], t[5])
    addInstruction(Set(t[0], '', t[5], None))

    
    
def p_identifier(t):
    'identifier                 : ID'
    addProduction('identifier → ID')
    simbol = ts.get(t[1])
    if simbol == None:
        print('No se encontro el simbolo :'+str(t[1]))
        t[0] = None
    else:
        t[0] = simbol.name
    
    
def p_identifier_array(t):
    'identifier                 : ID bracket'
    addProduction('identifier → ID bracket')
    arr = ts.get(t[1]).name
    t[0] = arr+'['+str(t[2][0])+']'

    
def p_brackets(t):
    'brackets                   : bracket'
    addProduction('brackets → bracket')
    t[0] = t[1]
    
def p_brackets_empty(t):
    'brackets                   : COR_IZQ COR_DER'
    addProduction('brackets → COR_IZQ COR_DER')
    t[0] = 0
    
def p_bracket(t):
    'bracket                    : bracket COR_IZQ expression COR_DER'
    addProduction('bracket → bracket COR_IZQ expression COR_DER')
    t[1].append(t[3])
    t[0] = t[1]

def p_bracket_end(t):
    'bracket                    : COR_IZQ expression COR_DER'
    addProduction('bracket → COR_IZQ expression COR_DER')
    t[0] = [t[2]]
    
def p_assignation(t):
    'assignation                : identifier equals expression'
    addProduction('assignation → identifier equals expression')
    t[0] = t[1]
    addInstruction(Set(t[0], '', t[3], None))
    
def p_assignation_struct(t):
    'assignation                : identifier PUNTO ID equals expression'
    addProduction('assignation → identifier PUNTO ID equals expression')
    t[0] = t[1]+'["'+t[3]+'"]'
    addInstruction(Set(t[0], '', t[5], None))

    
def p_equals(t):
    '''equals                   : IGUAL
			                    | SUMA_IGUAL
			                    | RESTA_IGUAL
			                    | MULT_IGUAL
			                    | DIV_IGUAL
			                    | MOD_IGUAL
			                    | SHIFT_IZQ_IGUAL
			                    | SHIFT_DER_IGUAL
			                    | AND_BIT_IGUAL
                                | XOR_BIT_IGUAL	
                                | OR_BIT_IGUAL'''
    if t[1] == '=':
        addProduction('equals → IGUAL')
    elif t[1] == '+=':
        addProduction('equals → SUMA_IGUAL')
    elif t[1] == '-=':
        addProduction('equals → RESTA_IGUAL')
    elif t[1] == '*=':
        addProduction('equals → MULT_IGUAL')
    elif t[1] == '/=':
        addProduction('equals → DIV_IGUAL')
    elif t[1] == '%=':
        addProduction('equals → MOD_IGUAL')
    elif t[1] == '<<=':
        addProduction('equals → SHIFT_IZQ_IGUAL')
    elif t[1] == '>>=':
        addProduction('equals → SHIFT_DER_IGUAL')
    elif t[1] == '&=':
        addProduction('equals → AND_BIT_IGUAL')
    elif t[1] == '^=':
        addProduction('equals → XOR_BIT_IGUAL')
    elif t[1] == '|=':
        addProduction('equals → OR_BIT_IGUAL')

def p_if(t):
    'if                         : IF PAR_IZQ expression PAR_DER if_marcker function_block add_if_label'
    addProduction('if → IF PAR_IZQ expression PAR_DER function_block')   
    addLabel(Label(t[5], []))
    global ifLabels
    global labels
    for ifs in ifLabels:
        labels[ifs].instructions = labels[ifs].instructions + [Goto(t[5])]
    deleteIfLabel()

def p_add_if_label(t):
    'add_if_label               : '
    addIfLabel()

def p_if_marcker(t):
    'if_marcker                 : '
    t[0] = getIf()
    addInstruction(If('!'+t[-2], t[0]))

def p_if_else_if(t):
    'if                         : IF PAR_IZQ expression PAR_DER if_marcker function_block add_if_label ELSE else_marcker if'
    addProduction('if → IF PAR_IZQ expression PAR_DER function_block ELSE if')
    t[0] = t[9]

def p_else_marcker(t):
    'else_marcker               : '
    addLabel(Label(t[-4], []))

def p_if_else(t):
    'if                         : IF PAR_IZQ expression PAR_DER if_marcker function_block add_if_label ELSE else_marcker function_block'
    addProduction('if → IF PAR_IZQ expression PAR_DER function_block ELSE function_block')
    then = getIf()
    addInstruction(Goto(then))
    addLabel(Label(then, []))
    global ifLabels
    global labels
    for ifs in ifLabels:
        labels[ifs].instructions = labels[ifs].instructions + [Goto(then)]
    deleteIfLabel()


def p_while(t):
    'while                      : WHILE label_add PAR_IZQ expression PAR_DER while_add function_block'
    addProduction('while → WHILE PAR_IZQ expression PAR_IZQ function_block')
    addInstruction(Goto(t[2]))
    addLabel(Label(t[6], []))


def p_while_add(t):
    'while_add                  : '
    t[0] = getWhile()
    addInstruction(If('!'+t[-2], t[0]))


def p_label_add(t):
    'label_add                  : '
    if t[-1] == 'while':
        t[0] = getWhile()
    elif t[-1] == 'do':
        t[0] = getDo()  
    elif t[-4] == 'for':
        t[0] = getFor()

    addInstruction(Goto(t[0]))
    addLabel(Label(t[0], []))  

def p_do(t):
    'do                         : DO label_add function_block WHILE PAR_IZQ expression PAR_DER'
    addProduction('do → DO function_block WHILE PAR_IZQ expression PAR_DER')
    addInstruction(If(t[6], t[2]))
    goto = getDo()  
    addInstruction(Goto(goto))
    addLabel(Label(goto, [])) 

def p_for(t):
    'for                        : FOR PAR_IZQ for_declaration PUNTO_COMA label_add expression PUNTO_COMA step_marcker step PAR_DER do_marcker function_block'
    addProduction('for → FOR PAR_IZQ for_declaration PUNTO_COMA expression PUNTO_COMA step PAR_DER function_block')
    addInstruction(Goto(t[8][0]))
    addLabel(Label(t[8][2], []))

def p_do_marcker(t):
    'do_marcker                 : '
    addInstruction(Goto(t[-6]))
    addLabel(Label(t[-3][1], []))  

def p_step_marcker(t):
    'step_marcker               : '
    t[0] = [getFor(), getFor(), getFor()]
    addInstruction(If(t[-2], t[0][1]))
    addInstruction(Goto(t[0][2]))
    addLabel(Label(t[0][0], []))  

def p_for_declaration_declaration(t):
    'for_declaration            : declaration'
    addProduction('for_declaration → declaration')
    t[0] = t[1]

def p_for_declaration_assignation(t):
    'for_declaration            : assignation'
    addProduction('for_declaration → assignation')
    t[0] = t[1]

def p_step_assigantion(t):
    'step                       : assignation'
    addProduction('step → assignation')
    t[0] = t[1]

def p_step_increase(t):
    'step                       : increase'
    addProduction('step → increase')
    t[0] = t[1]

def p_step_decrease(t):
    'step                       : decrease'
    addProduction('step → decrease')
    t[0] = t[1]

def p_switch(t):
    'switch                     : SWITCH PAR_IZQ expression PAR_DER BRA_IZQ cases BRA_DER'
    addProduction('switch → SWITCH PAR_IZQ expression PAR_DER BRA_IZQ cases BRA_DER')

    
def p_cases(t):
    'cases                      : cases case'
    addProduction('cases → cases case')

def p_cases_default(t):
    'cases                      : cases default'
    addProduction('cases → cases default')

def p_cases_end(t):
    'cases                      : case'
    addProduction('cases → case')

def p_case(t):
    'case                       : CASE expression DOS_PUNTOS instructions'
    addProduction('case → CASE expression DOS_PUNTOS instructions')

def p_default(t):
    'default                    : DEFAULT DOS_PUNTOS instructions'
    addProduction('default → DEFAULT DOS_PUNTOS instructions')

def p_label(t):
    'label                      : ID DOS_PUNTOS'
    addProduction('label → ID DOS_PUNTOS')
    addInstruction(Goto(t[1]))
    addLabel(Label(t[1], []))

def p_goto(t):
    'goto                       : GOTO ID'
    addProduction('goto → GOTO ID')
    addInstruction(Goto(t[2]))

def p_break(t):
    'break                      : BREAK'
    addProduction('break → BREAK')

def p_continue(t):
    'continue                   : CONTINUE'
    addProduction('continue → CONTINUE')

def p_return(t):
    'return                     : RETURN expression'
    addProduction('return → RETURN expression')
    t[0] = getReturn()
    addInstruction(Set(t[0], '', t[2], None))
    global actualFunction
    addInstruction(Goto(getCallReturn(actualFunction)))


def p_return_empty(t):
    'return                     : RETURN'
    addProduction('return → RETURN')
    global actualFunction
    addInstruction(Goto(getCallReturn(actualFunction)))

def p_function_call(t):
    'function_call              : ID param_val'
    addProduction('function_call → ID param_val')
    if t[1] == 'printf':
        te = t[2][0][1:-1]
        texto = re.split("%.", te)
        cont = 1
        for tex in texto:
            addInstruction(Print('"'+tex+'"'))
            if cont < len(t[2]):
                addInstruction(Print(t[2][cont]))
                cont += 1

        t[0] == '0'
    elif t[1] == 'scanf':
        t[0] = getTemp()
        addInstruction(Set(t[0], '', 'read()', ''))

    else:
        par = getFunctionParams(t[1])
        cont = 0
        for param in par:
            addInstruction(Set(par[param].name, '', t[2][cont], None))
            cont += 1

        callInd = getCallIndex()
        addInstruction(Set('$ra['+str(getFunctionCallIndex(t[1]))+']', '', str(callInd), None))
        addInstruction(Goto(t[1]))
        callName = getCallName(t[1])
        addLabel(Label(callName, [])) 
        addCall(t[1], Call(callInd, callName))
        t[0] = getReturnName(t[1])
        


def p_param_val_empty(t):
    'param_val                  : PAR_IZQ PAR_DER'
    addProduction('param_val → PAR_IZQ PAR_DER')
    t[0] = []

def p_param_val(t):
    'param_val                  : PAR_IZQ expression_list PAR_DER'
    addProduction('param_val → PAR_IZQ expression_list PAR_DER')
    t[0] = t[2]

def p_expression_list(t):
    'expression_list            : expression_list COMA expression'
    addProduction('expression_list → expression_list COMA expression')
    t[1].append(t[3])
    t[0] = t[1]

def p_expression_list_end(t):
    'expression_list            : expression'
    addProduction('expression_list → expression')
    t[0] = [t[1]]

def p_expression(t):
    '''expression               : expression SUMA expression
                                | expression RESTA expression
                                | expression MULT expression
                                | expression DIV expression
                                | expression MOD expression
                                | expression AND expression
                                | expression OR expression
                                | expression ES_IGUAL expression
                                | expression NO_IGUAL expression
                                | expression MENOR_IGUAL expression
                                | expression MAYOR_IGUAL expression
                                | expression MENOR expression
                                | expression MAYOR expression
                                | expression AND_BIT expression
                                | expression OR_BIT expression
                                | expression XOR_BIT expression
                                | expression SHIFT_IZQ expression
                                | expression SHIFT_DER expression
                                | RESTA expression %prec MENOS
                                | AND_BIT expression %prec REFERENCIA
                                | NOT expression
                                | NOT_BIT expression
                                | PAR_IZQ expression PAR_DER
                                | expression INTERROGACION expression DOS_PUNTOS expression'''
    if t[2] == '+':
        addProduction('expression → expression SUMA expression')
        t[0] = getTemp()
        addInstruction(Set(t[0], '+', t[1], t[3]))

    elif t[2] == '-':
        addProduction('expression → expression RESTA expression')
        t[0] = getTemp()
        addInstruction(Set(t[0], '-', t[1], t[3]))

    elif t[2] == '*':
        addProduction('expression → expression MULT expression')
        t[0] = getTemp()
        addInstruction(Set(t[0], '*', t[1], t[3]))

    elif t[2] == '/':
        addProduction('expression → expression DIV expression')
        t[0] = getTemp()
        addInstruction(Set(t[0], '/', t[1], t[3]))

    elif t[2] == '%':
        addProduction('expression → expression MOD expression')
        t[0] = getTemp()
        addInstruction(Set(t[0], '%', t[1], t[3]))

    elif t[2] == '&&':
        addProduction('expression → expression AND expression')
        t[0] = getTemp()
        addInstruction(Set(t[0], '&&', t[1], t[3]))

    elif t[2] == '||':
        addProduction('expression → expression OR expression')
        t[0] = getTemp()
        addInstruction(Set(t[0], '||', t[1], t[3]))

    elif t[2] == '==':
        addProduction('expression → expression ES_IGUAL expression')
        t[0] = getTemp()
        addInstruction(Set(t[0], '==', t[1], t[3]))

    elif t[2] == '!=':
        addProduction('expression → expression NO_IGUAL expression')
        t[0] = getTemp()
        addInstruction(Set(t[0], '!=', t[1], t[3]))

    elif t[2] == '<=':
        addProduction('expression → expression MENOR_IGUAL expression')
        t[0] = getTemp()
        addInstruction(Set(t[0], '<=', t[1], t[3]))

    elif t[2] == '>=':
        addProduction('expression → expression MAYOR_IGUAL expression')
        t[0] = getTemp()
        addInstruction(Set(t[0], '>=', t[1], t[3]))

    elif t[2] == '<':
        addProduction('expression → expression MENOR expression')
        t[0] = getTemp()
        addInstruction(Set(t[0], '<', t[1], t[3]))

    elif t[2] == '>':
        addProduction('expression → expression MAYOR expression')
        t[0] = getTemp()
        addInstruction(Set(t[0], '>', t[1], t[3]))

    elif t[2] == '&':
        addProduction('expression → expression AND_BIT expression')
        t[0] = getTemp()
        addInstruction(Set(t[0], '&', t[1], t[3]))

    elif t[2] == '|':
        addProduction('expression → expression OR_BIT expression')
        t[0] = getTemp()
        addInstruction(Set(t[0], '|', t[1], t[3]))

    elif t[2] == '^':
        addProduction('expression → expression XOR_BIT expression')
        t[0] = getTemp()
        addInstruction(Set(t[0], '^', t[1], t[3]))

    elif t[2] == '<<':
        addProduction('expression → expression SHIFT_IZQ expression')
        t[0] = getTemp()
        addInstruction(Set(t[0], '<<', t[1], t[3]))

    elif t[2] == '>>':
        addProduction('expression → expression SHIFT_DER expression')
        t[0] = getTemp()
        addInstruction(Set(t[0], '>>', t[1], t[3]))

    elif t[2] == '?':
        addProduction('expression → expression INTERROGACION expression DOS_PUNTOS expression')

    elif t[1] == '-':
        addProduction('expression → RESTA expression')
        t[0] = getTemp()
        addInstruction(Set(t[0], '-', t[2], None))

    elif t[1] == '&':
        addProduction('expression → AND_BIT expression')
        t[0] = getTemp()
        addInstruction(Set(t[0], '&', t[2], None))

    elif t[1] == '!':
        addProduction('expression → NOT expression')
        t[0] = getTemp()
        addInstruction(Set(t[0], '!', t[2], None))

    elif t[1] == '~':
        addProduction('expression → NOT_BIT expression')
        t[0] = getTemp()
        addInstruction(Set(t[0], '~', t[2], None))

    elif t[1] == '(':
        addProduction('expression → PAR_IZQ expression PAR_DER')
        t[0] = t[2]

def p_expression_increase(t):
    'expression                 : increase'
    addProduction('expression → increase')
    t[0] = t[1]

def p_expression_decrease(t):
    'expression                 : decrease'
    addProduction('expression → decrease')
    t[0] = t[1]

def p_expression_entero(t):
    'expression                 : ENTERO'
    addProduction('expression → ENTERO')
    t[0] = str(t[1])

def p_expression_decimal(t):
    'expression                 : DECIMAL'
    addProduction('expression → DECIMAL')
    t[0] = str(t[1])

def p_expression_caracter(t):
    'expression                 : CARACTER'
    addProduction('expression → CARACTER')
    t[0] = t[1]

def p_expression_cadena(t):
    'expression                 : CADENA'
    addProduction('expression → CADENA')
    t[0] = t[1]

def p_expression_call(t):
    'expression                 : function_call'
    addProduction('expression → function_call')
    t[0] = t[1]

def p_expression_conversion(t):
    'expression                 : conversion'
    addProduction('expression → conversion')
    t[0] = t[1]

def p_expression_identifier(t):
    'expression                 : identifier'
    addProduction('expression → identifier')
    t[0] = t[1]

def p_struct_property(t):
    'expression                 : identifier PUNTO ID'
    addProduction('expression → identifier PUNTO ID')
    t[0] = t[1]+'["'+t[3]+'"]'

def p_conversion_int(t):
    'conversion                 : PAR_IZQ INT PAR_DER expression'
    addProduction('conversion → PAR_IZQ INT PAR_DER expression')
    t[0] = getTemp()
    addInstruction(Set(t[0], '(int)', t[4], None))

def p_conversion_float(t):
    'conversion                 : PAR_IZQ DOUBLE PAR_DER expression'
    addProduction('conversion → PAR_IZQ DOUBLE PAR_DER expression')
    t[0] = getTemp()
    addInstruction(Set(t[0], '(float)', t[4], None))

def p_conversion_char(t):
    'conversion                 : PAR_IZQ CHAR PAR_DER expression'
    addProduction('conversion → PAR_IZQ CHAR PAR_DER expression')
    t[0] = getTemp()
    addInstruction(Set(t[0], '(char)', t[4], None))

def p_increase_pre(t):
    'increase                   : SUMA_SUMA expression %prec PRE_INC'
    addProduction('increase → SUMA_SUMA expression')
    addInstruction(Set(t[1], '+', t[2], '1'))

def p_increase_post(t):
    'increase                   : expression SUMA_SUMA %prec POST_INC'
    addProduction('increase → expression SUMA_SUMA')
    addInstruction(Set(t[1], '+', t[1], '1'))

def p_decrease_pre(t):
    'decrease                   : RESTA_RESTA expression %prec PRE_DEC'
    addProduction('decrease → RESTA_RESTA expression')
    addInstruction(Set(t[1], '-', t[2], '1'))

def p_decrease_post(t):
    'decrease                   : expression RESTA_RESTA %prec POST_DEC'
    addProduction('decrease → expression RESTA_RESTA')
    addInstruction(Set(t[1], '-', t[1], '1'))

def p_error(t):
    #print('ERROR SINTACTICO: El simbolo "' + str(t.value) + '", identificador como '+str(t.type)+', no era el esperado en la linea '+ str(t.lineno) +'.\n')
    print(t)

def analizar(texto):
    lexer = lex.lex()
    lexer.input(texto)
    parser = yacc.yacc() 
    parser.parse(texto)   
    return traduce(labels)
    #print('ERROR NO CONTROLADO: A ocurrido un problema en el analisis lexico y sintactico. \n                   Para recivir soporte pongase en contacto con siguiente correo: jorgejuarezdal→gmail.com')



input = 'int a = 3; int b=a*a+a-2*a/(3-a)*(-1); void juas(){int i = 1; int a = 0; while(i){a += 1; if(a>10){i = 0;}}} int main(){ int j = 0; if(a) {j = 3;}} '
f = open("C:\\Users\\Jorge\\Documents\\proyectos\\OLC2\\[OLC2]Proyecto2_201807022\\Pruebas\\structs.mc", "r")
input = 'int main(){int a; if(1>2){a = 3;}else if(3*6>2){a = 100;} else if(3>2){a = 900000;} else{a = 3000;}  if(a == 3){char po = \'a\';}}'
input = 'int main(){int a = 1; if(a>0){printf("hola"); a = 4;}else{printf("adio");} }'
input = f.read()
analizar(input)
print(traduce(labels))