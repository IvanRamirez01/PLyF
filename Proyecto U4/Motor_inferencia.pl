% inference_engine.pl
% Motor de inferencia basado en hechos regla(Head, BodyList).
% - Soporta: conjunciones (lista de metas), built-ins (is, =, =:=, =\=, <, >, =<, >=),
%   negación por falla (\+), corte (!) y trazado sencillo.
% - Usa copy_term/2 para evitar captura de variables y permitir backtracking correcto.

:- module(inference_engine, [
    regla/2,
    inferir/1,
    resolver/1,
    resolver_traza/1,
    es_builtin/1
]).

/* ----------------------
   Utilidades
   ---------------------- */

% is_builtin(+Goal)
% Verdadero si Goal debe ser delegado al motor Prolog directamente (built-in).
es_builtin(Goal) :-
    callable(Goal),
    Goal =.. [F|_],
    member(F, [is, =, '=:=' , '=\\=', '<', '>', '=<', '>=', '\\+', '!' , member, nth0, write, writeln, format]).

% ejecutar_meta(+Goal)
% Si es builtin, delega a call/1; si es una negación \+(X) se evalúa adecuadamente.
ejecutar_meta(\+ G) :- !,
    (   \+ call(G)
    ->  true
    ;   fail
    ).
ejecutar_meta(!) :- !, !.  % el cut será ejecutado en el contexto de este meta-intérprete (cuidado con semántica)
ejecutar_meta(Goal) :-
    es_builtin(Goal), !,
    call(Goal).

/* ----------------------
   Resolver (lista de metas)
   ---------------------- */

% resolver(+Goals)
% Goals es una lista de metas por resolver. Ejemplo: resolver([op(suma,2,3,R)]).
resolver([]).
resolver([Meta | Resto]) :-
    % Si Meta es un built-in o negación, ejecutarlo directamente
    (   ejecutar_meta(Meta) ->
        resolver(Resto)
    ;   % Sino, buscar una regla aplicable
        % Tomamos una regla definida por regla(Head, BodyList)
        regla(Head, Body),
        % Hacemos una copia fresca para no mezclar variables entre alternativas
        copy_term((Head, Body), (HeadC, BodyC)),
        % Intentamos unificar la cabeza copiada con la meta actual
        HeadC = Meta,
        % BodyC debe ser una lista de metas; si es 'true' o [], se transforma a [].
        (   BodyC == true -> Nuevas = [] ; Nuevas = BodyC ),
        % Concatenamos las nuevas metas (del cuerpo) con las metas pendientes.
        append(Nuevas, Resto, Siguientes),
        % Resolver recursivamente (backtracking automático si falla más adelante)
        resolver(Siguientes)
    ).

/* ----------------------
   Versión con traza (Call/Exit)
   ---------------------- */

resolver_traza(Goals) :- resolver_traza(Goals, 0).

resolver_traza([], _) :- !.
resolver_traza([Meta | Resto], Nivel) :-
    tab(Nivel), format('Call: ~w~n', [Meta]),
    (   ejecutar_meta(Meta) ->
        tab(Nivel), format('Exit (builtin): ~w~n', [Meta]),
        resolver_traza(Resto, Nivel)
    ;   % intentar reglas
        regla(Head, Body),
        copy_term((Head, Body), (HeadC, BodyC)),
        (   catch(HeadC = Meta, _, fail) -> true ; fail ),
        (   BodyC == true -> Nuevas = [] ; Nuevas = BodyC ),
        append(Nuevas, Resto, Siguientes),
        NextNivel is Nivel + 2,
        resolver_traza(Siguientes, NextNivel),
        tab(Nivel), format('Exit: ~w~n', [Meta])
    ).

/* ----------------------
   Interfaz principal
   ---------------------- */

% inferir(+Meta). Sintaxis de uso: ?- inferir(op(suma,2,3,R)).
% Muestra soluciones mediante backtracking.
inferir(Meta) :-
    (   resolver([Meta]) ->
        % Mostrar las soluciones encontradas ya que Meta quedó unificada
        write('Solución: '), write(Meta), nl,
        fail  % forzar backtracking para mostrar siguientes soluciones
    ;   true
    ).
inferir(_).  % fin cuando no hay más soluciones

% KB ejemplo: reglas para la calculadora (usa regla/2)
% suma
regla(op(suma, X, Y, R), [R is X + Y]).

% resta
regla(op(resta, X, Y, R), [R is X - Y]).

% multiplicación
regla(op(multiplicacion, X, Y, R), [R is X * Y]).

% división (protección contra división por cero)
regla(op(division, X, Y, R), [Y =\= 0, R is X / Y]).

% factorial: definicion recursiva
regla(op(factorial, 0, _, 1), []).        % caso base: factorial(0)=1
regla(op(factorial, N, _, R), [N > 0, N1 is N - 1, op(factorial, N1, _, R1), R is N * R1]).


% Nota: inferir/1 muestra soluciones via write/1; también puedes usar resolver/1
% directamente y capturar variables en la sesión Prolog:
% ?- resolver([op(suma,2,3,R)]).
% R = 5.

