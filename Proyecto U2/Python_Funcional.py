from functools import reduce

# Simulación de un archivo de logs
logs = [
    "Error al conectar con la base de datos",
    "Conexión exitosa al servidor",
    "Error al leer el archivo",
    "Conexión cerrada correctamente"
]

# Funciones puras
def limpiar_texto(linea):
    return linea.lower().replace("al", "").replace("con", "").split()

def contar_palabras(dic, palabra):
    dic[palabra] = dic.get(palabra, 0) + 1
    return dic

# Pipeline funcional
palabras = map(limpiar_texto, logs)
lista_palabras = [p for linea in palabras for p in linea]
conteo = reduce(contar_palabras, lista_palabras, {})

print("Conteo de palabras:", conteo)
