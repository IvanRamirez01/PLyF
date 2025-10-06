logs = [
    "Error al conectar con la base de datos",
    "Conexión exitosa al servidor",
    "Error al leer el archivo",
    "Conexión cerrada correctamente"
]

conteo = {}

for linea in logs:
    linea = linea.lower().replace("al", "").replace("con", "")
    palabras = linea.split()
    for palabra in palabras:
        if palabra in conteo:
            conteo[palabra] += 1
        else:
            conteo[palabra] = 1

print("Conteo de palabras:", conteo)
