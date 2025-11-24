import math

def mostrar_menu():
    print("\n=== Calculadora Científica ===")
    print("1. Suma")
    print("2. Resta")
    print("3. Multiplicación")
    print("4. División")
    print("5. Potencia")
    print("6. Raíz cuadrada")
    print("7. Factorial")
    print("8. Salir")

def sumar(a, b):
    return a + b

def restar(a, b):
    return a - b

def multiplicar(a, b):
    return a * b

def dividir(a, b):
    if b == 0:
        print("Error: división entre cero no permitida.")
        return None
    return a / b

def potencia(a, b):
    return a ** b

def raiz_cuadrada(a):
    if a < 0:
        print("Error: no se puede calcular la raíz de un número negativo.")
        return None
    return math.sqrt(a)

def factorial(n):
    if n < 0:
        print("Error: factorial no definido para negativos.")
        return None
    resultado = 1
    for i in range(1, n + 1):
        resultado *= i
    return resultado

def ejecutar_calculadora():
    while True:
        mostrar_menu()
        opcion = input("Selecciona una opción (1-8): ")

        if opcion == '8':
            print("Saliendo de la calculadora...")
            break

        if opcion in ['1', '2', '3', '4', '5']:
            try:
                a = float(input("Ingresa el primer número: "))
                b = float(input("Ingresa el segundo número: "))
            except ValueError:
                print("Error: ingresa valores numéricos válidos.")
                continue

            if opcion == '1':
                print(f"Resultado: {sumar(a, b)}")
            elif opcion == '2':
                print(f"Resultado: {restar(a, b)}")
            elif opcion == '3':
                print(f"Resultado: {multiplicar(a, b)}")
            elif opcion == '4':
                resultado = dividir(a, b)
            elif opcion == '5':
                print(f"Resultado: {potencia(a, b)}")

        elif opcion == '6':
            try:
                a = float(input("Ingresa el número: "))
                resultado = raiz_cuadrada(a)
                if resultado is not None:
                    print(f"Resultado: {resultado}")
            except ValueError:
                print("Error: ingresa un valor numérico válido.")


        elif opcion == '7':
            try:
                n = int(input("Ingresa un número entero: "))
                resultado = factorial(n)
                if resultado is not None:
                    print(f"Resultado: {resultado}")
            except ValueError:
                print("Error: ingresa un número entero válido.")

        else:
            print("Opción inválida. Intenta nuevamente.")

if __name__ == "__main__":
    ejecutar_calculadora()