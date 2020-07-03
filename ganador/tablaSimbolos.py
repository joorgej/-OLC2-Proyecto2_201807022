class Simbolo():
    def __init__(self, index, name, valor, dimension = None):
        self.index = index
        self.name = name
        self.valor = valor
        self.dimension = dimension

class TablaSimbolos():
    def __init__(self, simbolos = {}):
        self.simbolos = simbolos        

    def get(self, key):
        simbolo = None
        try:
            simbolo = self.simbolos[key]
        except:
            simbolo = None
        if simbolo != None and len(simbolo) != 0:
            return simbolo[0]
        else:
            return None

    def add(self, key, name, val, dim = None):
        simbolo = None
        try:
            simbolo = self.simbolos[key]
        except:
            simbolo = None
        if simbolo == None or len(simbolo) == 0:
            self.simbolos[key] = [Simbolo(0, name, val, dim)]
        else:
            self.simbolos[key] = [Simbolo(len(simbolo), name, val, dim)]+self.simbolos[key]

    def delete(self, key):
        self.simbolos[key].pop(0)

    def graph(self):
        lineas = 'NO --- NOMBRE --- REGISTRO\n'
        cont = 1
        for simb in self.simbolos:
            for simbol in self.simbolos[simb]:
                lineas += str(cont)+' --- '+str(simb)+str(simbol.index)+' --- '+simbol.name+'\n'
                cont += 1
        lineas = lineas[:-1]+'}'
        return lineas
        