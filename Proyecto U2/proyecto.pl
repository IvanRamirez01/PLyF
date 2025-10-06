% -------- Hechos básicos de operaciones --------
operacion(suma).
operacion(resta).
operacion(multiplicacion).
operacion(division).
operacion(potencia).
operacion(raiz).
operacion(factorial).

% -------- Reglas de operaciones --------

% Suma
calcular(suma, X, Y, R) :- R is X + Y.

% Resta
calcular(resta, X, Y, R) :- R is X - Y.

% Multiplicación
calcular(multiplicacion, X, Y, R) :- R is X * Y.

% División (verifica que no haya división por cero)
calcular(division, X, Y, R) :- 
    Y =\= 0,
    R is X / Y.

% Potencia
calcular(potencia, X, Y, R) :- R is X ** Y.

% Raíz (usa exponente fraccionario)
calcular(raiz, X, Y, R) :-
    Y =\= 0,
    R is X ** (1 / Y).

% Factorial (definido recursivamente)
factorial(0, 1).
factorial(N, R) :-
    N > 0,
    N1 is N - 1,
    factorial(N1, R1),
    R is N * R1.

calcular(factorial, X, _, R) :-
    X >= 0,
    factorial(X, R).

% -------- Reglas auxiliares --------

% Regla general: evalúa la operación si existe
evaluar(Op, X, Y, Resultado) :-
    operacion(Op),
    calcular(Op, X, Y, Resultado), !.

% Si la operación no está definida
evaluar(Op, _, _, _) :-
    \+ operacion(Op),
    write('Error: operación no reconocida.'), nl, fail.

% -------- Ejemplos de uso --------
% ?- evaluar(suma, 5, 3, R).
% R = 8.
%
% ?- evaluar(potencia, 2, 3, R).
% R = 8.
%
% ?- evaluar(raiz, 27, 3, R).
% R = 3.0.
%
% ?- evaluar(factorial, 5, _, R).
% R = 120.
%
% ?- evaluar(division, 4, 0, R).
% false.