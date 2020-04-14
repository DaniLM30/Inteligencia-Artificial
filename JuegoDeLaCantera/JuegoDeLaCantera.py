# Algoritmo MinMax PECL2 Inteligencia Artificial
# Realizado por:
#   Daniel Lopez Moreno DNI: 03217279Q
#   Luis Alejandro Cabanillas Prudencio DNI: 04236930P
#   Alejandro Fernandez Maceira DNI: 03223178G

import random, time

DIFICULTAD = 3


def sorteo():
    """Devuelve un numero aleatorio entre 0 o 1"""
    return random.randrange(2)


def pide_dimensiones():
    """Pide las dimensiones para el bloque inicial y devuelve una lista"""
    print("Introduce la dimension x:")
    x = int(input())
    print("Introduce la dimension y:")
    y = int(input())
    print("Introduce la dimension z:")
    z = int(input())

    cubo = (x, y, z)
    return cubo


def movimiento_jugador(cubo):
    """Pedimos al jugador por donde quiere cortar"""
    print("Por que coordenada deseas hacer el corte: ")
    coordenada = input()
    print("Por que altura deseas cortar: ")
    altura = int(input())

    if coordenada == 'x' or coordenada == 'X':
        if altura < (cubo[0]):
            nuevo_cubo = (altura, cubo[1], cubo[2])  # Cortamos el cubo por donde ha elegido el jugador
            return nuevo_cubo
        else:
            print("Altura no valida")
            return movimiento_jugador(cubo)

    elif coordenada == 'y' or coordenada == 'Y':
        if altura < (cubo[1]):
            nuevo_cubo = (cubo[0], altura, cubo[2])  # Cortamos el cubo por donde ha elegido el jugador
            return nuevo_cubo
        else:
            print("Altura no valida")
            return movimiento_jugador(cubo)

    elif coordenada == 'z' or coordenada == 'Z':
        if altura < (cubo[2]):
            nuevo_cubo = (cubo[0], cubo[1], altura)  # Cortamos el cubo por donde ha elegido el jugador
            return nuevo_cubo
        else:
            print("Altura no valida")
            return movimiento_jugador(cubo)

    else:
        print("Coordenada no valida")  # Si ninguna coordenada es valida, volvemos a pedir
        movimiento_jugador(cubo)


def movimiento_pc(tablero):
    """Realiza el movimiento del pc en el tablero"""
    t0 = time.time()  # inicio cronometro

    a = minimax(tablero, False, DIFICULTAD)[2]  # algoritmo minimax limitado segun la dificultad

    if (a is not None):
        tablero = minimax(tablero, False, DIFICULTAD)[1]  # sustituye la posicion a en el tablero por su letra

    print("La maquina ha tardado {:.5f} ms".format((time.time() - t0) * 1000))  # tiempo desde inicio del cronometro
    return tablero


def ganador(cubo, turno_player):
    """Si el tamanio del cubo es 1,1,1 devolvemos como
        ganador al jugador que lo ha dejado asi"""
    cubo_ganador = (1, 1, 1)
    if cubo == cubo_ganador:
        return turno_player  #Si hemos llegado a 1,1,1 devolvemos el turno del ganador
    else:
        return -1  # Si no -1


def minimax(tablero, turno_player, profundidad):
    """Implementacion del algoritmo minimax a nuestro juego.
        ARGUMENTOS:
            -tablero. Lista con el tamanio de las 3 coordenadas.
            -turno_player. Booleano que indica el turno, si es positivo significa que le toca al jugador humano.
            -profundidad. Valor numerico que limita el numero de veces que la funcion se llama a si misma (dificultad) y que incita a la maquina
                          a realizar los movimientos que impliquen alargar la partida lo maximo posible (intentando ganar siempre)."""

    if (ganador(tablero, turno_player) == 1):
        return (+10 - profundidad, tablero, None)  # gana pc

    elif (ganador(tablero, turno_player) == 0):
        return (-10 - profundidad, tablero, None)  # pierde pc

    elif (turno_player):
        best = (+11, tablero, None)

        best1 = best
        best2 = best
        best3 = best

        for a in range(1, tablero[0]):  # Comprobamos el mejor camino cambiando la coordenada x
            if (tablero[0] != 1):
                valor1 = minimax((a, tablero[1], tablero[2]), not turno_player, profundidad - 1)[0]
                tablero1 = (a, tablero[1], tablero[2])
                if (valor1 < best1[0]):
                    best1 = (valor1, tablero1, a)  # jugador intenta causar el MENOR beneficio a pc

        for b in range(1,tablero[1]):  # Comprobamos el mejor camino cambiando la coordenada y
            if (tablero[1] != 1):
                valor2 = minimax((tablero[0], b, tablero[2]), not turno_player, profundidad - 1)[0]
                tablero2 = (tablero[0], b, tablero[2])
                if (valor2 < best2[0]):
                    best2 = (valor2, tablero2, b)  # jugador intenta causar el MENOR beneficio a pc

        for c in range(1,tablero[2]):  # Comprobamos el mejor camino cambiando la coordenada z
            if (tablero[2] != 1):
                valor3 = minimax((tablero[0], tablero[1], c), not turno_player, profundidad - 1)[0]
                tablero3 = (tablero[0], tablero[1], c)
                if (valor3 < best3[0]):
                    best3 = (valor3, tablero3, c)  # jugador intenta causar el MENOR beneficio a pc

        # Comprobamos cual de los 3 coordenadas es mejor cambiar
        if (best1[0] < best2[0]):
            if (best1[0] < best3[0]):
                return best1
            else:
                return best3
        elif (best2[0] < best3[0]):
            return best2
        else:
            return best3

    else:  # turno de pc
        best = (-11, tablero, None)
        best1 = best
        best2 = best
        best3 = best

        for a in range(1,tablero[0]):  # Comprobamos el mejor camino cambiando la coordenada x
            if (tablero[0] != 1):
                valor1 = minimax((a, tablero[1], tablero[2]), not turno_player, profundidad - 1)[0]
                tablero1 = (a, tablero[1], tablero[2])
                if (valor1 > best1[0]):
                    best1 = (valor1, tablero1, a)  # pc intenta causar el MAYOR beneficio a si mismo

        for b in range(1,tablero[1]):  # Comprobamos el mejor camino cambiando la coordenada y
            if (tablero[1] != 1):
                valor2 = minimax((tablero[0], b, tablero[2]), not turno_player, profundidad - 1)[0]
                tablero2 = (tablero[0], b, tablero[2])
                if (valor2 > best2[0]):
                    best2 = (valor2, tablero2, b)  # pc intenta causar el MAYOR beneficio a si mismo

        for c in range(1, tablero[2]):  # Comprobamos el mejor camino cambiando la coordenada z
            if (tablero[2] != 1):
                valor3 = minimax((tablero[0], tablero[1], c), not turno_player, profundidad - 1)[0]
                tablero3 = (tablero[0], tablero[1], c)
                if (valor3 > best3[0]):
                    best3 = (valor3, tablero3, c)  # pc intenta causar el MAYOR beneficio a si mismo

        # Comprobamos cual de los 3 coordenadas es mejor cambiar
        if(best1[0] > best2[0]):
            if(best1[0] > best3[0]):
                return best1
            else:
                return best3
        elif (best2[0] > best3[0]):
            return best2
        else:
            return best3


def cambio_turno(turno):
    """Cambiamos el turno de juego"""
    if turno == 0:
        return 1
    else:
        return 0


# Bucle principal del juego
salir = 0
turno = sorteo()
print("Empieza: "+str(turno))
cubo = pide_dimensiones()

while salir != -1:

    if turno == 0:
        print("Turno del Jugador")
        cubo = movimiento_jugador(cubo)
        print(cubo)

    elif turno == 1:
        print("Turno de la Maquina")
        cubo = movimiento_pc(cubo)
        print(cubo)

    if ganador(cubo, turno) == 0:
        print("Gana el humano")
        salir = -1
    elif ganador(cubo, turno) == 1:
        print("Gana la maquina")
        salir = -1

    turno = (cambio_turno(turno))

