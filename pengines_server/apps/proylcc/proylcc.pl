:- module(proylcc, 
	[  
		join/4,
		booster/3
	]).

/**
 * join(+Grid, +NumOfColumns, +Path, -RGrids) 
 * RGrids es la lista de grillas representando el efecto, en etapas, de combinar las celdas del camino Path
 * en la grilla Grid, con número de columnas NumOfColumns. El número 0 representa que la celda está vacía. 
 */ 
join(Grid, NumOfColumns, Path, RGrids) :-
	positionPath(Path, NumOfColumns, PositionPath),
	lastBlockValue(Grid, PositionPath, LastBlockValue),
	gridWithEmptyPath(Grid, PositionPath, 0, GridWithEmptyPath),
	last(PositionPath, LastPathPosition),
	replaceValueInGridPosition(GridWithEmptyPath, LastPathPosition, LastBlockValue, GridWithLastBlock),
	gridWithGravity(GridWithLastBlock, NumOfColumns, GridWithGravity),
	RGrids = [GridWithEmptyPath, GridWithLastBlock, GridWithGravity].

/**
 * En base al listado de [X, Y] de elementos del camino, devuelve una lista de enteros que corresponden
 * a los índices/posiciones en la grilla.
 */
positionPath(Path, NumOfColumns, PositionPath) :-
    positionPathAux(Path, NumOfColumns, [], PositionPath).

positionPathAux([], _, PositionPath, PositionPath).

positionPathAux([[N | Ns] | T], NumOfColumns, PositionPath, Result) :-
    Position is N * NumOfColumns + Ns,
    append(PositionPath, [Position] , PositionPathAppended),
    positionPathAux(T, NumOfColumns, PositionPathAppended, Result).


/**
 * Recorre la grilla y verifica si la posición actual de la grilla es miembro de la lista de posiciones del camino.
 * En caso de serlo, se modifica el valor del elemento por 0.
 */
gridWithEmptyPath([_N], PositionPath, CurrentPosition, EmptyGrid) :- 
	member(CurrentPosition, PositionPath),
	EmptyGrid = [0].

gridWithEmptyPath([N], PositionPath, CurrentPosition, EmptyGrid) :- 
	\+member(CurrentPosition, PositionPath),
	EmptyGrid = [N].

gridWithEmptyPath([_N | Ns], PositionPath, CurrentPosition, EmptyGrid) :- 
    NextPos is CurrentPosition + 1,
	member(CurrentPosition, PositionPath),
	gridWithEmptyPath(Ns, PositionPath, NextPos, EmptyGridAux),
	append([0], EmptyGridAux, EmptyGrid).

gridWithEmptyPath([N | Ns], PositionPath, CurrentPosition, EmptyGrid) :- 
    NextPos is CurrentPosition + 1,
	\+member(CurrentPosition, PositionPath),
	gridWithEmptyPath(Ns, PositionPath, NextPos, EmptyGridAux),
	append([N], EmptyGridAux, EmptyGrid).

/**
 * Devuelve la suma de valores de la grilla en un determinado camino 
 */
sumOfValuesInPath(Grid, PositionPath, Result) :-
	sumOfValuesInPathAux(Grid, PositionPath, 0, 0, Result).

sumOfValuesInPathAux([], _, _, SummedValues, SummedValues).

sumOfValuesInPathAux([G | Gs], PositionPath, CurrentGridPosition, SummedValues, Result) :-
	NextGridPosition is CurrentGridPosition + 1,
	(member(CurrentGridPosition, PositionPath) ->
		SummedValuesAux is SummedValues + G,
		sumOfValuesInPathAux(Gs, PositionPath, NextGridPosition, SummedValuesAux, Result)
	;
		sumOfValuesInPathAux(Gs, PositionPath, NextGridPosition, SummedValues, Result)
	).
	
/**
 * Devuelve el valor que debería tener el bloque resultante
 */
lastBlockValue(Grid, PositionPath, LastBlockValue) :-
	sumOfValuesInPath(Grid, PositionPath, SumOfPathValue),
	smallerPow2GreaterOrEqualThan(SumOfPathValue, LastBlockValue).

/**
 * Calcula la potencia de dos mas cercana o igual a un determinado numero
 */
smallerPow2GreaterOrEqualThan(Num, Result) :-
	Result is 2 ^ ceil(log(Num) / log(2)).

/**
 * Devuelve una grilla luego de aplicar gravedad
 */
gridWithGravity(GridWithEmptyPath, NumOfColumns, Result) :-
	reverse(GridWithEmptyPath, ReversedGrid),
	gridWithGravityAppliedAux(ReversedGrid, NumOfColumns, 0, [], ResultAux1),
    reverse(ResultAux1, ResultAux2),
	replaceZerosWithRandomValue(ResultAux2, Result).

gridWithGravityAppliedAux([], _, _, GridWithGravity, Result) :-
	Result = GridWithGravity.

gridWithGravityAppliedAux(Grid, NumOfColumns, CurrentPosition, NewGrid, Result) :-
	Grid = [CurrentBlock | T],
	NextPosition is CurrentPosition + 1,
	% Si el bloque actual está vacío
	(CurrentBlock =:= 0 ->
		aboveBlockPosition(Grid, NumOfColumns, AboveBlockPosition),
		nth0(AboveBlockPosition, Grid, AboveBlockValue),
		replaceValueInGridPosition(Grid, AboveBlockPosition, 0, GridWithAboveBlockReplaced),
        append(NewGrid, [AboveBlockValue], NewGridAppended),
		GridWithAboveBlockReplaced = [_ | TAux],
		gridWithGravityAppliedAux(TAux, NumOfColumns, NextPosition, NewGridAppended, Result)
	;
		append(NewGrid, [CurrentBlock], NewGridAppended),
		gridWithGravityAppliedAux(T, NumOfColumns, NextPosition, NewGridAppended, Result)
	).

/**
 * Devuelve la posición del bloque no vacío más cercano dentro de la misma columna.
 * Si no existe un bloque por encima no vacío, devuelve 0
 */
aboveBlockPosition(Grid, NumOfColumns, Result) :-
	aboveBlockPositionAux(Grid, NumOfColumns, NumOfColumns, Result).

aboveBlockPositionAux(Grid, CurrentPosition, _, Result) :-
	length(Grid, GridLength),
    CurrentPosition >= GridLength,
    Result = 0.

aboveBlockPositionAux(Grid, CurrentPosition, NumOfColumns, Result) :-
	nth0(CurrentPosition, Grid, Value),
	NextPosition is CurrentPosition + NumOfColumns,
	((Value =:= 0) ->
		aboveBlockPositionAux(Grid, NextPosition, NumOfColumns, Result)
	;
		Result = CurrentPosition
	).

/**
 * Devuelve una lista que reemplaza el valor en cierta posición por un nuevo valor
 */
replaceValueInGridPosition(List, Position, NewValue, Result) :-
	nth0(Position, List, _, ListRemainder),
	nth0(Position, Result, NewValue, ListRemainder).

/**
 * Devuelve una lista que reemplaza todos los ceros por valores aleatorios que sean potencia de 2
 */
replaceZerosWithRandomValue([N], Result) :-
	((N =:= 0) ->
		generateRandomValue(X),
		Result = [X]
	;
		Result = [N]
	).

replaceZerosWithRandomValue([N | Ns], Result) :-
	replaceZerosWithRandomValue(Ns, ResultAux),
	((N =:= 0) ->
		generateRandomValue(X),
		append([X], ResultAux, Result)
	;
		append([N], ResultAux, Result)
	).

/**
 * Genera un valor aleatorio que sea potencia de 2
 */
generateRandomValue(Num) :-
	random(1, 7, X),
	pow(2, X, Num).

/**
 * Devuelve una lista de grillas que representan el efecto, en etapas, de eliminar todos los grupos,
 * poner los bloques correspondientes a cada grupo y aplicar la gravedad.
 */
booster(Grid, NumOfColumns, RGrids) :-
	getGroupList(Grid, Grid, NumOfColumns, 0, [], GroupList),
	flatten(GroupList, GroupListFlattened),
	valuesFromGroupsList(Grid, GroupList, [], NewBlocksList),
	gridWithEmptyPath(Grid, GroupListFlattened, 0, GridWithEmptyPath),
	gridWithBoosterBlocks(GridWithEmptyPath, GroupList, NewBlocksList, GridWithBoosterBlocks),
	gridWithGravity(GridWithBoosterBlocks, NumOfColumns, GridWithGravity),
	RGrids = [GridWithEmptyPath, GridWithBoosterBlocks, GridWithGravity].

/**
 * Devuelve la grilla con los bloques generados por cada grupo eliminado
 */
gridWithBoosterBlocks(Grid, [], [], Grid).

gridWithBoosterBlocks(Grid, [G | Gs], [S | Ss], EmptyBoosterGridWithBlocks) :-
	max_list(G, BlockIndex),
	replaceValueInGridPosition(Grid, BlockIndex, S, NewGrid),
	gridWithBoosterBlocks(NewGrid, Gs, Ss, EmptyBoosterGridWithBlocks).

/**
 * Devuelve una lista con los valores de los bloques a generar correspondientes a cada grupo.
 */
valuesFromGroupsList(Grid, [], ValuesList, ValuesList).

valuesFromGroupsList(Grid, [G | Gs], ValuesList, Res) :-
	sumOfValuesInPath(Grid, G, SumOfPathValue),
	smallerPow2GreaterOrEqualThan(SumOfPathValue, NewValue),
	append(ValuesList, [NewValue], ValuesListAux),
	valuesFromGroupsList(Grid, Gs, ValuesListAux, Res).

/**
 * Devuelve la lista de grupos adyacentes en la grilla. 
 * Cada grupo es una lista de índices/posiciones de bloques adyacentes que conforman un grupo.
 */
getGroupList([], _GridOriginal, _NumOfColumns, _CurrentPosition, GroupList, GroupList).

getGroupList(Grid, GridOriginal, NumOfColumns, CurrentPosition, GroupList, Res) :-
	Grid = [N | Ns],
    length(GridOriginal, GridLength),
	NewPosition is CurrentPosition + 1,
	flatten(GroupList, GroupListFlattened),
	(member(CurrentPosition, GroupListFlattened) ->
		getGroupList(Ns, GridOriginal, NumOfColumns, NewPosition, GroupList, Res) 	
	;
		adjacentPositionsList(CurrentPosition, GridLength, NumOfColumns, GridOriginal, [1,2,3,4,5,6,7,8], N, [], Group),
		length(Group, Length), 
		(Length > 1 -> 
			append([Group], GroupList, GroupListAux),
			getGroupList(Ns, GridOriginal, NumOfColumns, NewPosition, GroupListAux, Res)
           
        ;	
			getGroupList(Ns, GridOriginal, NumOfColumns, NewPosition, GroupList, Res)
		)
	).

/**
 * Dada una posición, devuelve un listado de índices/posiciones que conforman su grupo.
 * En caso de que no hayan adyacentes con valores iguales, devuelve una lista conformada 
 * únicamente por la posición actual.
 */
adjacentPositionsList(_CurrentPosition, _GridLength, _NumOfColumns, _GridOriginal, [], _Valor, _Group, []).

adjacentPositionsList(CurrentPosition, GridLength, NumOfColumns, GridOriginal, [X | Xs], Value, Group, Res) :-
	X =:= 1, 
    append([CurrentPosition], Group, GroupAux),
    
	((checkAdjacentRight(CurrentPosition, NumOfColumns),
		X1 is CurrentPosition + 1,
		(
		(\+member(X1, GroupAux),
		nth0(X1, GridOriginal, Elem1),
		Elem1 =:= Value,
		adjacentPositionsList(X1, GridLength, NumOfColumns, GridOriginal, [1,2,3,4,5,6,7,8], Value, GroupAux, GroupRes),
		adjacentPositionsList(CurrentPosition, GridLength, NumOfColumns, GridOriginal, Xs, Value, GroupRes, ResAux))
		;
		adjacentPositionsList(CurrentPosition, GridLength, NumOfColumns, GridOriginal, Xs, Value, GroupAux, ResAux)
		)
	);
		adjacentPositionsList(CurrentPosition, GridLength, NumOfColumns, GridOriginal, Xs, Value, GroupAux, ResAux)
			
	),
    Res = ResAux.
    
adjacentPositionsList(CurrentPosition, GridLength, NumOfColumns, GridOriginal, [X | Xs], Value, Group, Res) :-
	X =:= 2,
	((checkAdjacentLeft(CurrentPosition, NumOfColumns),
		X2 is CurrentPosition - 1,
		(
		(\+member(X2, Group),
		nth0(X2, GridOriginal, Elem1), 
		Elem1 =:= Value, 
		adjacentPositionsList(X2, GridLength, NumOfColumns, GridOriginal, [1,2,3,4,5,6,7,8], Value, Group, GroupRes),
		adjacentPositionsList(CurrentPosition, GridLength, NumOfColumns, GridOriginal, Xs, Value, GroupRes, ResAux))
		;
		adjacentPositionsList(CurrentPosition, GridLength, NumOfColumns, GridOriginal, Xs, Value, Group, ResAux)
		)
	);
		adjacentPositionsList(CurrentPosition, GridLength, NumOfColumns, GridOriginal, Xs, Value, Group, ResAux)
	),
    Res = ResAux.

adjacentPositionsList(CurrentPosition, GridLength, NumOfColumns, GridOriginal, [X | Xs], Value, Group, Res) :-
   	X =:= 3,
	((checkAdjacentTop(CurrentPosition, NumOfColumns),
		X3 is CurrentPosition - NumOfColumns,
		(
		(\+member(X3, Group),
		nth0(X3, GridOriginal, Elem1), 
		Elem1 =:= Value, 
		adjacentPositionsList(X3, GridLength, NumOfColumns, GridOriginal, [1,2,3,4,5,6,7,8], Value, Group, GroupRes),
		adjacentPositionsList(CurrentPosition, GridLength, NumOfColumns, GridOriginal, Xs, Value, GroupRes, ResAux))
		;
		adjacentPositionsList(CurrentPosition, GridLength, NumOfColumns, GridOriginal, Xs, Value, Group, ResAux)
		)
	);
		adjacentPositionsList(CurrentPosition, GridLength, NumOfColumns, GridOriginal, Xs, Value, Group, ResAux)
	),
    Res = ResAux.

adjacentPositionsList(CurrentPosition, GridLength, NumOfColumns, GridOriginal, [X | Xs], Value, Group, Res) :-
    X =:= 4,
	((checkAdjacentBottom(CurrentPosition, GridLength, NumOfColumns),
		X4 is CurrentPosition + NumOfColumns,
		(
		(\+member(X4, Group),
		nth0(X4, GridOriginal, Elem1), 
		Elem1 =:= Value, 
		adjacentPositionsList(X4, GridLength, NumOfColumns, GridOriginal, [1,2,3,4,5,6,7,8], Value, Group, GroupRes),
		adjacentPositionsList(CurrentPosition, GridLength, NumOfColumns, GridOriginal, Xs, Value, GroupRes, ResAux))
		;
		adjacentPositionsList(CurrentPosition, GridLength, NumOfColumns, GridOriginal, Xs, Value, Group, ResAux)
		)
	);
		adjacentPositionsList(CurrentPosition, GridLength, NumOfColumns, GridOriginal, Xs, Value, Group, ResAux)
	),
    Res = ResAux.


adjacentPositionsList(CurrentPosition, GridLength, NumOfColumns, GridOriginal, [X | Xs], Value, Group, Res) :-
    X =:= 5, 
	((checkAdjacentBottomRight(CurrentPosition, GridLength, NumOfColumns),
		X5 is CurrentPosition + NumOfColumns + 1,
		(
		(\+member(X5, Group),
		nth0(X5, GridOriginal, Elem1), 
		Elem1 =:= Value, 
		adjacentPositionsList(X5, GridLength, NumOfColumns, GridOriginal, [1,2,3,4,5,6,7,8], Value, Group, GroupRes),
		adjacentPositionsList(CurrentPosition, GridLength, NumOfColumns, GridOriginal, Xs, Value, GroupRes, ResAux))
		;
		adjacentPositionsList(CurrentPosition, GridLength, NumOfColumns, GridOriginal, Xs, Value, Group, ResAux)
		)
	);
		adjacentPositionsList(CurrentPosition, GridLength, NumOfColumns, GridOriginal, Xs, Value, Group, ResAux)
	),
    Res = ResAux.

adjacentPositionsList(CurrentPosition, GridLength, NumOfColumns, GridOriginal, [X | Xs], Value, Group, Res) :-
    X =:= 6, 
	((checkAdjacentBottomLeft(CurrentPosition, GridLength, NumOfColumns),
		X6 is CurrentPosition + NumOfColumns - 1,
		(
		(\+member(X6, Group),
		nth0(X6, GridOriginal, Elem1), 
		Elem1 =:= Value, 
		adjacentPositionsList(X6, GridLength, NumOfColumns, GridOriginal, [1,2,3,4,5,6,7,8], Value, Group, GroupRes),
		adjacentPositionsList(CurrentPosition, GridLength, NumOfColumns, GridOriginal, Xs, Value, GroupRes, ResAux))
		;
		adjacentPositionsList(CurrentPosition, GridLength, NumOfColumns, GridOriginal, Xs, Value, Group, ResAux)
		)
	);
		adjacentPositionsList(CurrentPosition, GridLength, NumOfColumns, GridOriginal, Xs, Value, Group, ResAux)
	), 
    Res = ResAux.
    
adjacentPositionsList(CurrentPosition, GridLength, NumOfColumns, GridOriginal, [X | Xs], Value, Group, Res) :-
    X =:= 7,
	((checkAdjacentTopRight(CurrentPosition, NumOfColumns),
		X7 is CurrentPosition - NumOfColumns + 1,
		(
		(\+member(X7, Group),
		nth0(X7, GridOriginal, Elem1), 
		Elem1 =:= Value, 
		adjacentPositionsList(X7, GridLength, NumOfColumns, GridOriginal, [1,2,3,4,5,6,7,8], Value, Group, GroupRes),
		adjacentPositionsList(CurrentPosition, GridLength, NumOfColumns, GridOriginal, Xs, Value, GroupRes, ResAux))
		;
		adjacentPositionsList(CurrentPosition, GridLength, NumOfColumns, GridOriginal, Xs, Value, Group, ResAux)
		)
	);
		adjacentPositionsList(CurrentPosition, GridLength, NumOfColumns, GridOriginal, Xs, Value, Group, ResAux)
	),
   Res = ResAux.

adjacentPositionsList(CurrentPosition, GridLength, NumOfColumns, GridOriginal, [X | Xs], Value, Group, Res) :-
    X =:= 8,
	((checkAdjacentTopLeft(CurrentPosition, NumOfColumns),
		X8 is CurrentPosition - NumOfColumns - 1,
		(
		(\+member(X8, Group),
		nth0(X8, GridOriginal, Elem1), 
		Elem1 =:= Value, 
		adjacentPositionsList(X8, GridLength, NumOfColumns, GridOriginal, [1,2,3,4,5,6,7,8], Value, Group, GroupRes),
		adjacentPositionsList(CurrentPosition, GridLength, NumOfColumns, GridOriginal, Xs, Value, GroupRes, ResAux))
		;
		adjacentPositionsList(CurrentPosition, GridLength, NumOfColumns, GridOriginal, Xs, Value, Group, ResAux)
		)
	);
		adjacentPositionsList(CurrentPosition, GridLength, NumOfColumns, GridOriginal, Xs, Value, Group, ResAux)
	), 
    append(Group, ResAux, Res).

/**
 * Dada una posición, verifica si existe un bloque a su derecha.
 */

checkAdjacentRight(CurrentPosition, NumOfColumns) :-
	RightPosition is CurrentPosition + 1,
	RightPosition mod NumOfColumns =\= 0.

/**
 * Dada una posición, verifica si existe un bloque a su izquierda.
 */

checkAdjacentLeft(CurrentPosition, NumOfColumns) :-
	CurrentPosition mod NumOfColumns =\= 0.

/**
 * Dada una posición, verifica si existe un bloque encima.
 */

checkAdjacentTop(CurrentPosition, NumOfColumns) :-
	CurrentPosition >= NumOfColumns.

/**
 * Dada una posición, verifica si existe un bloque debajo.
 */
checkAdjacentBottom(CurrentPosition, GridLength, NumOfColumns) :-
	BottomPosition is CurrentPosition + NumOfColumns,
	BottomPosition < GridLength.

/**
 * Dada una posición, verifica si existe un bloque abajo a la derecha.
 */

checkAdjacentBottomRight(CurrentPosition, GridLength, NumOfColumns) :-
	checkAdjacentBottom(CurrentPosition, GridLength, NumOfColumns),
	checkAdjacentRight(CurrentPosition, NumOfColumns).

/**
 * Dada una posición, verifica si existe un bloque abajo a la izquierda.
 */

checkAdjacentBottomLeft(CurrentPosition, GridLength, NumOfColumns) :-
	checkAdjacentBottom(CurrentPosition, GridLength, NumOfColumns),
	checkAdjacentLeft(CurrentPosition, NumOfColumns).

/**
 * Dada una posición, verifica si existe un bloque arriba a la derecha.
 */

checkAdjacentTopRight(CurrentPosition, NumOfColumns) :-
	checkAdjacentTop(CurrentPosition, NumOfColumns),
	checkAdjacentRight(CurrentPosition, NumOfColumns).

/**
 * Dada una posición, verifica si existe un bloque arriba a la izquierda.
 */

checkAdjacentTopLeft(CurrentPosition, NumOfColumns) :-
	checkAdjacentTop(CurrentPosition, NumOfColumns),
	checkAdjacentLeft(CurrentPosition, NumOfColumns).