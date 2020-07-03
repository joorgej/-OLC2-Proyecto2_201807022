from instruction import *


def traduce(labels):
    codigo = ''
    for label in labels:
        codigo += label.name+':\n'
        for instruction in label.instructions:
            if isinstance(instruction, Set):
                if instruction.op2 == None:
                    codigo += '\t'+str(instruction.name)+' = '+str(instruction.simbol)+ str(instruction.op1)+';\n'
                else:
                    codigo += '\t'+str(instruction.name)+' = '+str(instruction.op1)+str(instruction.simbol)+str(instruction.op2)+';\n'
            elif isinstance(instruction, Goto):
                codigo += '\tgoto '+str(instruction.name)+';\n'

            elif isinstance(instruction, If):
                codigo += '\tif ('+str(instruction.condition)+') goto '+str(instruction.goto)+';\n'

            elif isinstance(instruction, Exit):
                codigo += '\texit;\n'

            elif isinstance(instruction, Print):
                codigo += '\tprint('+instruction.text.replace('\n', '\\n').replace('\\t', '\t')+');\n'

        codigo += '\n'

    return codigo
