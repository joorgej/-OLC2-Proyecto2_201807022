class Parametro():
    def __init__(self, index, name):
        self.index = index
        self.name = name

class Call():
    def __init__(self, index, label):
        self.index = index
        self.label = label

class Funcion():
    def __init__(self, name, parameters, call_index, calls = None, returns = None):
        self.name = name
        self.parameters = {}
        for param in parameters:
            self.parameters[param.index] = param
        self.calls = calls
        self.returns = returns
        self.return_call = name+'_return_call'
        self.call_index = call_index
        

    def newCall(self, call):
        self.calls = self.calls + [call]


    
