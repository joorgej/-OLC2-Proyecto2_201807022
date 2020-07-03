class Instruction():
    '''clase padre'''

class Set(Instruction):
    def __init__(self, name, simbol, op1, op2):
        self.name = name
        self.simbol = simbol
        self.op1 = op1
        self.op2 = op2

class Unset(Instruction):
    def __init__(self, name):
        self.name = name

class If(Instruction):
    def __init__(self, condition, goto):
        self.condition = condition
        self.goto = goto

class Label(Instruction):
    def __init__(self, name, instructions = []):
        self.name = name
        self.instructions = instructions

class Goto(Instruction):
    def __init__(self, name):
        self.name = name

class Print(Instruction):
    def __init__(self, text):
        self.text = text

class Exit(Instruction):
    '''Exit instruction'''
