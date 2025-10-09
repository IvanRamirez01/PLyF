% -----------------------------
% Motor de inferencia básico
% -----------------------------

% Base de conocimiento (cláusulas de Horn)
regla(op(suma, X, Y, R), [R is X + Y]).
regla(op(resta, X, Y, R), [R is X - Y]).
regla(op(multiplicacion, X, Y, R), [R is X * Y]).
regla(op(division, X, Y, R), [Y \= 0, R is X / Y]).
regla(op(factorial, 0, 1), []).
regla(op(factorial, N, R), [N > 0, N1 is N - 1, op(factorial, N1, R1), R is N * R1]).

% Meta principal: ejecutar una consulta aplicando resolución sobre las reglas
inferir(Meta) :-
    resolver([Meta]).

% Caso base: si no quedan metas por resolver, se ha encontrado una solución
resolver([]).
resolver([Meta | Resto]) :-
    regla(Meta, Cuerpo),        % Encuentra una regla que unifique con la meta
    append(Cuerpo, Resto, NuevasMetas),
    resolver(NuevasMetas).      % Resuelve recursivamente las submetas

% -----------------------------
% Ejemplo de uso:
% ?- inferir(op(suma, 3, 5, R)).
% R = 8.
%
% ?- inferir(op(factorial, 4, R)).
% R = 24.
% -----------------------------
