# factorial_imperativo.py
def factorial(n):
    resultado = 1
    for i in range(1, n+1):
        resultado *= i
    return resultado

if __name__ == "__main__":
    import time
    inicio = time.time()
    print(f"Factorial de 5: {factorial(5)}")
    fin = time.time()
    print(f"Tiempo de ejecución: {fin - inicio:.6f} segundos")
