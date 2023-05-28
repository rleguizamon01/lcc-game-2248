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
	indexPath(Path, NumOfColumns, IndexPath),
	lastBlockValue(Grid, IndexPath, LastBlockValue),
	gridWithEmptyPath(Grid, IndexPath, 0, GridWithEmptyPath),
	last(IndexPath, LastPathIndex),
	replaceValueInGridIndex(GridWithEmptyPath, LastPathIndex, LastBlockValue, GridWithLastBlock),
	gridWithGravity(GridWithLastBlock, NumOfColumns, GridWithGravity),
	RGrids = [GridWithEmptyPath, GridWithLastBlock, GridWithGravity].

/**
 * indexPath(+Path, +NumOfColumns, -IndexPath)
 * En base al listado de [X, Y] de elementos del camino, devuelve una lista de enteros que corresponden
 * a los índices en la grilla.
 */
indexPath(Path, NumOfColumns, IndexPath) :-
    indexPathAux(Path, NumOfColumns, [], IndexPath).

indexPathAux([], _, IndexPath, IndexPath).

indexPathAux([[N | Ns] | T], NumOfColumns, IndexPath, Result) :-
    Index is N * NumOfColumns + Ns,
    append(IndexPath, [Index] , IndexPathAppended),
    indexPathAux(T, NumOfColumns, IndexPathAppended, Result).


/**
 *	gridWithEmptyPath(+Grid, +IndexPath, +CurrentIndex, -EmptyGrid) 
 *	Recorre la grilla y verifica si el índice actual de la grilla es miembro de la lista de índices del camino.
 * 	En caso de serlo, se modifica el valor del elemento por 0.
 */
gridWithEmptyPath([_N], IndexPath, CurrentIndex, EmptyGrid) :- 
	member(CurrentIndex, IndexPath),
	EmptyGrid = [0].

gridWithEmptyPath([N], IndexPath, CurrentIndex, EmptyGrid) :- 
	\+member(CurrentIndex, IndexPath),
	EmptyGrid = [N].

gridWithEmptyPath([_N | Ns], IndexPath, CurrentIndex, EmptyGrid) :- 
    NextIndex is CurrentIndex + 1,
	member(CurrentIndex, IndexPath),
	gridWithEmptyPath(Ns, IndexPath, NextIndex, EmptyGridAux),
	append([0], EmptyGridAux, EmptyGrid).

gridWithEmptyPath([N | Ns], IndexPath, CurrentIndex, EmptyGrid) :- 
    NextIndex is CurrentIndex + 1,
	\+member(CurrentIndex, IndexPath),
	gridWithEmptyPath(Ns, IndexPath, NextIndex, EmptyGridAux),
	append([N], EmptyGridAux, EmptyGrid).

/**
 * sumOfValuesInPath(+Grid, +IndexPath, -Result)
 * Devuelve la suma de valores de la grilla en un determinado camino 
 */
sumOfValuesInPath(Grid, IndexPath, Result) :-
	sumOfValuesInPathAux(Grid, IndexPath, 0, 0, Result).

sumOfValuesInPathAux([], _, _, SummedValues, SummedValues).

sumOfValuesInPathAux([G | Gs], IndexPath, CurrentGridIndex, SummedValues, Result) :-
	NextGridIndex is CurrentGridIndex + 1,
	(
		(member(CurrentGridIndex, IndexPath),
		SummedValuesAux is SummedValues + G,
		sumOfValuesInPathAux(Gs, IndexPath, NextGridIndex, SummedValuesAux, Result))
	;
		(\+member(CurrentGridIndex, IndexPath),
		sumOfValuesInPathAux(Gs, IndexPath, NextGridIndex, SummedValues, Result))
	).
	
/**
 * lastBlockValue(+Grid, +IndexPath, -LastBlockValue)
 * Devuelve el valor que debería tener el bloque resultante
 */
lastBlockValue(Grid, IndexPath, LastBlockValue) :-
	sumOfValuesInPath(Grid, IndexPath, SumOfPathValue),
	smallerPow2GreaterOrEqualThan(SumOfPathValue, LastBlockValue).

/**
 * smallerPow2GreaterOrEqualThan(+Num, -Result)
 * Calcula la potencia de dos mas cercana o igual a un determinado numero
 */
smallerPow2GreaterOrEqualThan(Num, Result) :-
	Result is 2 ^ ceil(log(Num) / log(2)).

/**
 * gridWithGravity(+GridWithEmptyPath, +NumOfColumns, -Result)
 * Devuelve una grilla luego de aplicar gravedad
 */
gridWithGravity(GridWithEmptyPath, NumOfColumns, Result) :-
	reverse(GridWithEmptyPath, ReversedGrid),
	gridWithGravityAppliedAux(ReversedGrid, NumOfColumns, 0, [], ResultAux1),
    reverse(ResultAux1, ResultAux2),
	replaceZerosWithRandomValue(ResultAux2, Result).

gridWithGravityAppliedAux([], _, _, GridWithGravity, Result) :-
	Result = GridWithGravity.

gridWithGravityAppliedAux(Grid, NumOfColumns, CurrentIndex, NewGrid, Result) :-
	Grid = [CurrentBlock | T],
	NextIndex is CurrentIndex + 1,
	% Si el bloque actual está vacío
	(
		(CurrentBlock =:= 0,
		aboveBlockIndex(Grid, NumOfColumns, AboveBlockIndex),
		nth0(AboveBlockIndex, Grid, AboveBlockValue),
		replaceValueInGridIndex(Grid, AboveBlockIndex, 0, GridWithAboveBlockReplaced),
        append(NewGrid, [AboveBlockValue], NewGridAppended),
		GridWithAboveBlockReplaced = [_ | TAux],
		gridWithGravityAppliedAux(TAux, NumOfColumns, NextIndex, NewGridAppended, Result))
	;
		(CurrentBlock =\= 0,
		append(NewGrid, [CurrentBlock], NewGridAppended),
		gridWithGravityAppliedAux(T, NumOfColumns, NextIndex, NewGridAppended, Result))
	).

/**
 * aboveBlockIndex(+Grid, +NumOfColumns, -Result)
 * Devuelve el índice del bloque no vacío más cercano dentro de la misma columna.
 * Si no existe un bloque por encima no vacío, devuelve 0
 */
aboveBlockIndex(Grid, NumOfColumns, Result) :-
	aboveBlockIndexAux(Grid, NumOfColumns, NumOfColumns, Result).

aboveBlockIndexAux(Grid, CurrentIndex, _, Result) :-
	length(Grid, GridLength),
    CurrentIndex >= GridLength,
    Result = 0.

aboveBlockIndexAux(Grid, CurrentIndex, NumOfColumns, Result) :-
	nth0(CurrentIndex, Grid, Value),
	NextIndex is CurrentIndex + NumOfColumns,
	(
		(Value =:= 0,
		aboveBlockIndexAux(Grid, NextIndex, NumOfColumns, Result))
	;
		(Value =\= 0,
		Result = CurrentIndex)
	).

/**
 * replaceValueInGridIndex(+List, +Index, +NewValue, -Result)
 * Devuelve una lista que reemplaza el valor en cierto índice por un nuevo valor
 */
replaceValueInGridIndex(List, Index, NewValue, Result) :-
	nth0(Index, List, _, ListRemainder),
	nth0(Index, Result, NewValue, ListRemainder).

/**
 * replaceZerosWithRandomValue(+[N], -Result)
 * Devuelve una lista que reemplaza todos los ceros por valores aleatorios que sean potencia de 2
 */
replaceZerosWithRandomValue([N], Result) :-
	(
		(N =:= 0,
		generateRandomValue(X),
		Result = [X])
	;
		(N =\= 0,
		Result = [N])
	).

replaceZerosWithRandomValue([N | Ns], Result) :-
	replaceZerosWithRandomValue(Ns, ResultAux),
	(
		(N =:= 0,
		generateRandomValue(X),
		append([X], ResultAux, Result))
	;
		(N =\= 0,
		append([N], ResultAux, Result))
	).

/**
 * generateRandomValue(-Num)
 * Genera un valor aleatorio que sea potencia de 2
 */
generateRandomValue(Num) :-
	random(1, 7, X),
	pow(2, X, Num).

/**
 * booster(+Grid, +NumOfColumns, -RGrids)
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
 * gridWithBoosterBlocks(+Grid, +[G | Gs], +[S | Ss], -GridWithBoosterBlocks)
 * Devuelve la grilla con los bloques generados por cada grupo eliminado
 */
gridWithBoosterBlocks(Grid, [], [], Grid).

gridWithBoosterBlocks(Grid, [G | Gs], [S | Ss], GridWithBoosterBlocks) :-
	max_list(G, BlockIndex),
	replaceValueInGridIndex(Grid, BlockIndex, S, NewGrid),
	gridWithBoosterBlocks(NewGrid, Gs, Ss, GridWithBoosterBlocks).

/**
 * valuesFromGroupsList(+Grid, +[G | Gs], +ValuesList, -Res)
 * Devuelve una lista con los valores de los bloques a generar correspondientes a cada grupo.
 */
valuesFromGroupsList(_, [], ValuesList, ValuesList).

valuesFromGroupsList(Grid, [G | Gs], ValuesList, Res) :-
	sumOfValuesInPath(Grid, G, SumOfPathValue),
	smallerPow2GreaterOrEqualThan(SumOfPathValue, NewValue),
	append(ValuesList, [NewValue], ValuesListAux),
	valuesFromGroupsList(Grid, Gs, ValuesListAux, Res).

/**
 * getGroupList(+Grid, +GridOriginal, +NumOfColumns, +CurrentIndex, +GroupList, -Res)
 * Devuelve la lista de grupos adyacentes en la grilla. 
 * Cada grupo es una lista de índices de bloques adyacentes que conforman un grupo.
 */
getGroupList([], _, _, _, Res, Res).

getGroupList(Grid, GridOriginal, NumOfColumns, CurrentIndex, GroupList, Res) :-
	Grid = [N | Ns],
    length(GridOriginal, GridLength),
	NewIndex is CurrentIndex + 1,
	flatten(GroupList, GroupListFlattened),
	(
		(member(CurrentIndex, GroupListFlattened),
		getGroupList(Ns, GridOriginal, NumOfColumns, NewIndex, GroupList, Res))
	;
		(\+member(CurrentIndex, GroupListFlattened),
		adjacentIndexesList(CurrentIndex, GridLength, NumOfColumns, GridOriginal, N, [], Group),
		length(Group, Length), 
		(
			(Length > 1,
			append([Group], GroupList, GroupListAux),
			getGroupList(Ns, GridOriginal, NumOfColumns, NewIndex, GroupListAux, Res))
        ;	
			(Length =< 1,
			getGroupList(Ns, GridOriginal, NumOfColumns, NewIndex, GroupList, Res))
		))
	).

/**
 * adjacentIndexesList(+CurrentIndex, +GridLength, +NumOfColumns, +GridOriginal, +Value, +Group, -Res)
 * Dado un índice, devuelve un listado de índices que conforman su grupo.
 * En caso de que no hayan adyacentes con valores iguales, devuelve una lista conformada 
 * únicamente por los índices actuales.
 */

 adjacentIndexesList(CurrentIndex, GridLength, NumOfColumns, GridOriginal, Value, Group, Res) :-
    append([CurrentIndex], Group, GroupAppended),
	(	checkAdjacentRight(CurrentIndex, NumOfColumns),
		X1 is CurrentIndex + 1,
		adjacentIndexesListAux(X1, CurrentIndex, GridLength, NumOfColumns, GridOriginal, Value, GroupAppended, Res1)
	;	Res1 = GroupAppended
	),
	(	checkAdjacentLeft(CurrentIndex, NumOfColumns),
		X2 is CurrentIndex - 1,
		adjacentIndexesListAux(X2, CurrentIndex, GridLength, NumOfColumns, GridOriginal, Value, Res1, Res2)
	;	Res2 = Res1
	),
	(	checkAdjacentTop(CurrentIndex, NumOfColumns),
		X3 is CurrentIndex - NumOfColumns,
		adjacentIndexesListAux(X3, CurrentIndex, GridLength, NumOfColumns, GridOriginal, Value, Res2, Res3)
	;	Res3 = Res2
	),
	(	checkAdjacentBottom(CurrentIndex, GridLength, NumOfColumns),
		X4 is CurrentIndex + NumOfColumns,
		adjacentIndexesListAux(X4, CurrentIndex, GridLength, NumOfColumns, GridOriginal, Value, Res3, Res4)
	;	Res4 = Res3
	),
	(	checkAdjacentBottomRight(CurrentIndex, GridLength, NumOfColumns),
		X5 is CurrentIndex + NumOfColumns + 1,
		adjacentIndexesListAux(X5, CurrentIndex, GridLength, NumOfColumns, GridOriginal, Value, Res4, Res5)
	;	Res5 = Res4
	),
	(	checkAdjacentBottomLeft(CurrentIndex, GridLength, NumOfColumns),
		X6 is CurrentIndex + NumOfColumns - 1,
		adjacentIndexesListAux(X6, CurrentIndex, GridLength, NumOfColumns, GridOriginal, Value, Res5, Res6)
	;	Res6 = Res5
	), 
	(	checkAdjacentTopRight(CurrentIndex, NumOfColumns),
		X7 is CurrentIndex - NumOfColumns + 1,
		adjacentIndexesListAux(X7, CurrentIndex, GridLength, NumOfColumns, GridOriginal, Value, Res6, Res7)
	;	Res7 = Res6
	),
	(	checkAdjacentTopLeft(CurrentIndex, NumOfColumns),
		X8 is CurrentIndex - NumOfColumns - 1,
		adjacentIndexesListAux(X8, CurrentIndex, GridLength, NumOfColumns, GridOriginal, Value, Res7, Res)
	;	Res = Res7
	).

adjacentIndexesListAux(XCheck, _, GridLength, NumOfColumns, GridOriginal, Value, Group, Res) :-
	\+member(XCheck, Group),
	nth0(XCheck, GridOriginal, Elem1), 
	Elem1 =:= Value, 
	adjacentIndexesList(XCheck, GridLength, NumOfColumns, GridOriginal, Value, Group, Res).


/**
 * checkAdjacentRight(+CurrentIndex, +NumOfColumns)
 * Dado un índice, verifica si existe un bloque a su derecha.
 */
checkAdjacentRight(CurrentIndex, NumOfColumns) :-
	RightIndex is CurrentIndex + 1,
	RightIndex mod NumOfColumns =\= 0.

/**
 * checkAdjacentLeft(+CurrentIndex, +NumOfColumns)
 * Dado un índice, verifica si existe un bloque a su izquierda.
 */
checkAdjacentLeft(CurrentIndex, NumOfColumns) :-
	CurrentIndex mod NumOfColumns =\= 0.

/**
 * checkAdjacentTop(+CurrentIndex, +NumOfColumns)
 * Dado un índice, verifica si existe un bloque encima.
 */
checkAdjacentTop(CurrentIndex, NumOfColumns) :-
	CurrentIndex >= NumOfColumns.

/**
 * checkAdjacentBottom(+CurrentIndex, +GridLength, +NumOfColumns) 
 * Dado un índice, verifica si existe un bloque debajo.
 */
checkAdjacentBottom(CurrentIndex, GridLength, NumOfColumns) :-
	BottomIndex is CurrentIndex + NumOfColumns,
	BottomIndex < GridLength.

/**
 * checkAdjacentBottomRight(+CurrentIndex, +GridLength, +NumOfColumns)
 * Dado un índice, verifica si existe un bloque abajo a la derecha.
 */
checkAdjacentBottomRight(CurrentIndex, GridLength, NumOfColumns) :-
	checkAdjacentBottom(CurrentIndex, GridLength, NumOfColumns),
	checkAdjacentRight(CurrentIndex, NumOfColumns).

/**
 * checkAdjacentBottomLeft(+CurrentIndex, +GridLength, +NumOfColumns)
 * Dado un índice, verifica si existe un bloque abajo a la izquierda.
 */
checkAdjacentBottomLeft(CurrentIndex, GridLength, NumOfColumns) :-
	checkAdjacentBottom(CurrentIndex, GridLength, NumOfColumns),
	checkAdjacentLeft(CurrentIndex, NumOfColumns).

/**
 * checkAdjacentTopRight(+CurrentIndex, +NumOfColumns)
 * Dado un índice, verifica si existe un bloque arriba a la derecha.
 */
checkAdjacentTopRight(CurrentIndex, NumOfColumns) :-
	checkAdjacentTop(CurrentIndex, NumOfColumns),
	checkAdjacentRight(CurrentIndex, NumOfColumns).

/**
 * checkAdjacentTopLeft(+CurrentIndex, +NumOfColumns)
 * Dado un índice, verifica si existe un bloque arriba a la izquierda.
 */
checkAdjacentTopLeft(CurrentIndex, NumOfColumns) :-
	checkAdjacentTop(CurrentIndex, NumOfColumns),
	checkAdjacentLeft(CurrentIndex, NumOfColumns).


/**
*
* PARTE 2 --------------------------------------------------------------------
*
*/
getBestPathInGrid(_CurrentIndex, [], _CopyGrid, _GridLength, _NumOfColumns, PreviousBestPath, PreviousBestPath).

getBestPathInGrid(CurrentIndex, [_X | Xs], CopyGrid, GridLength, NumOfColumns, PreviousBestPath, GetRes) :-
	nth0(CurrentIndex, CopyGrid, Value),
	adjacentIndexesList2(CurrentIndex, GridLength, NumOfColumns, CopyGrid, [1,2,3,4,5,6,7,8], [], [], _Res, [], Paths),
	pathWithBestScore(Paths, CopyGrid, BestPath),
	pathWithBestScore([BestPath, PreviousBestPath], CopyGrid, NewBestPath),
	NewIndex is CurrentIndex + 1,
	getBestPathInGrid(NewIndex, Xs, CopyGrid, GridLength, NumOfColumns, NewBestPath, GetRes).

/*

*/
pathWithBestScore(Paths, Grid, Res) :-
	pathWithBestScoreAux(Paths, Grid, [], 0, Res).

pathWithBestScoreAux([], _, BestPath, _, BestPath).
pathWithBestScoreAux([P | Ps], Grid, BestPath, BestPathScore, Res) :-
	valuePath(P, Grid, [], ValuePath),
	sum_list(ValuePath, ValuePathSummed),
	(
		ValuePathSummed > BestPathScore,
		pathWithBestScoreAux(Ps, Grid, P, ValuePathSummed, Res)
	;
		pathWithBestScoreAux(Ps, Grid, BestPath, BestPathScore, Res)
	).

valuePath([], _Grid, ValuePath, ValuePath).
valuePath([I | Is], Grid, ValuePath, Res) :-
	nth0(I, Grid, Value),
	append(ValuePath, [Value], ValuePathAppended),
	valuePath(Is, Grid, ValuePathAppended, Res).
/*

*/

adjacentIndexesList2(_CurrentIndex, _GridLength, _NumOfColumns, _GridOriginal, [], _Group, _Path, [], ListaCaminos, ListaCaminos).

adjacentIndexesList2(CurrentIndex, GridLength, NumOfColumns, GridOriginal, [X | Xs], Group, Path, Res, ListaCaminos, ListaCaminosRes) :-
	X =:= 1, 
    append([CurrentIndex], Group, GroupAux),
    append([CurrentIndex], Path, PathAux),
	((checkAdjacentRight(CurrentIndex, NumOfColumns),
		X1 is CurrentIndex + 1,
		(
		(\+member(X1, GroupAux),
		nth0(X1, GridOriginal, Elem1),
        nth0(CurrentIndex, GridOriginal, Value),
		(
			Elem1 =:= Value;
			length(PathAux, LengthPath),
			LengthPath > 1,
			Elem1 =:= Value * 2
		),
		adjacentIndexesList2(X1, GridLength, NumOfColumns, GridOriginal, [1,2,3,4,5,6,7,8], GroupAux, PathAux, _GroupRes, ListaCaminos, ListaCaminosAux),
		adjacentIndexesList2(CurrentIndex, GridLength, NumOfColumns, GridOriginal, Xs, GroupAux, PathAux, ResAux, ListaCaminosAux, ListaCaminosRes))
		;
		adjacentIndexesList2(CurrentIndex, GridLength, NumOfColumns, GridOriginal, Xs, GroupAux, PathAux, ResAux, ListaCaminos, ListaCaminosRes)
		)
	);
		adjacentIndexesList2(CurrentIndex, GridLength, NumOfColumns, GridOriginal, Xs, GroupAux, PathAux, ResAux, ListaCaminos, ListaCaminosRes)
			
	),
    Res = ResAux.
    
adjacentIndexesList2(CurrentIndex, GridLength, NumOfColumns, GridOriginal, [X | Xs], Group, Path, Res, ListaCaminos, ListaCaminosRes) :-
	X =:= 2,
	((checkAdjacentLeft(CurrentIndex, NumOfColumns),
		X2 is CurrentIndex - 1,
		adjacentIndexesListAux2(X2, CurrentIndex, GridLength, NumOfColumns, GridOriginal, [X | Xs], Group, Path, ResAux, ListaCaminos, ListaCaminosAux)
	);
		adjacentIndexesList2(CurrentIndex, GridLength, NumOfColumns, GridOriginal, Xs, Group, Path, ResAux, ListaCaminos, ListaCaminosAux)
	),
    ListaCaminosRes = ListaCaminosAux,
    Res = ResAux.

adjacentIndexesList2(CurrentIndex, GridLength, NumOfColumns, GridOriginal, [X | Xs], Group, Path, Res, ListaCaminos, ListaCaminosRes) :-
   	X =:= 3,
	((checkAdjacentTop(CurrentIndex, NumOfColumns),
		X3 is CurrentIndex - NumOfColumns,
		adjacentIndexesListAux2(X3, CurrentIndex, GridLength, NumOfColumns, GridOriginal, [X | Xs], Group, Path, ResAux, ListaCaminos, ListaCaminosAux)
	);
		adjacentIndexesList2(CurrentIndex, GridLength, NumOfColumns, GridOriginal, Xs, Group, Path, ResAux, ListaCaminos, ListaCaminosAux)
	),
    ListaCaminosRes = ListaCaminosAux,
    Res = ResAux.

adjacentIndexesList2(CurrentIndex, GridLength, NumOfColumns, GridOriginal, [X | Xs], Group, Path, Res, ListaCaminos, ListaCaminosRes) :-
    X =:= 4,
	((checkAdjacentBottom(CurrentIndex, GridLength, NumOfColumns),
		X4 is CurrentIndex + NumOfColumns,
		adjacentIndexesListAux2(X4, CurrentIndex, GridLength, NumOfColumns, GridOriginal, [X | Xs], Group, Path, ResAux, ListaCaminos, ListaCaminosAux)
	);
		adjacentIndexesList2(CurrentIndex, GridLength, NumOfColumns, GridOriginal, Xs, Group, Path, ResAux, ListaCaminos, ListaCaminosAux)
	),
    ListaCaminosRes = ListaCaminosAux,
    Res = ResAux.


adjacentIndexesList2(CurrentIndex, GridLength, NumOfColumns, GridOriginal, [X | Xs], Group, Path, Res, ListaCaminos, ListaCaminosRes) :-
    X =:= 5,
	((checkAdjacentBottomRight(CurrentIndex, GridLength, NumOfColumns),
		X5 is CurrentIndex + NumOfColumns + 1,
		adjacentIndexesListAux2(X5, CurrentIndex, GridLength, NumOfColumns, GridOriginal, [X | Xs], Group, Path, ResAux, ListaCaminos, ListaCaminosAux)
	);
		adjacentIndexesList2(CurrentIndex, GridLength, NumOfColumns, GridOriginal, Xs, Group, Path, ResAux, ListaCaminos, ListaCaminosAux)
	),
    ListaCaminosRes = ListaCaminosAux,
    Res = ResAux.

adjacentIndexesList2(CurrentIndex, GridLength, NumOfColumns, GridOriginal, [X | Xs], Group, Path, Res, ListaCaminos, ListaCaminosRes) :-
    X =:= 6,
	((checkAdjacentBottomLeft(CurrentIndex, GridLength, NumOfColumns),
		X6 is CurrentIndex + NumOfColumns - 1,
		adjacentIndexesListAux2(X6, CurrentIndex, GridLength, NumOfColumns, GridOriginal, [X | Xs], Group, Path, ResAux, ListaCaminos, ListaCaminosAux)
	);
		adjacentIndexesList2(CurrentIndex, GridLength, NumOfColumns, GridOriginal, Xs, Group, Path, ResAux, ListaCaminos, ListaCaminosAux)
	),
    ListaCaminosRes = ListaCaminosAux,
    Res = ResAux.
    
adjacentIndexesList2(CurrentIndex, GridLength, NumOfColumns, GridOriginal, [X | Xs], Group, Path, Res, ListaCaminos, ListaCaminosRes) :-
    X =:= 7,
	((checkAdjacentTopRight(CurrentIndex, NumOfColumns),
		X7 is CurrentIndex - NumOfColumns + 1,
		adjacentIndexesListAux2(X7, CurrentIndex, GridLength, NumOfColumns, GridOriginal, [X | Xs], Group, Path, ResAux, ListaCaminos, ListaCaminosAux)
	);
		adjacentIndexesList2(CurrentIndex, GridLength, NumOfColumns, GridOriginal, Xs, Group, Path, ResAux, ListaCaminos, ListaCaminosAux)
	),
   ListaCaminosRes = ListaCaminosAux,
   Res = ResAux.

adjacentIndexesList2(CurrentIndex, GridLength, NumOfColumns, GridOriginal, [X | Xs], Group, Path, Res, ListaCaminos, ListaCaminosRes) :-
    X =:= 8,
	((checkAdjacentTopLeft(CurrentIndex, NumOfColumns),
		X8 is CurrentIndex - NumOfColumns - 1,
		adjacentIndexesListAux2(X8, CurrentIndex, GridLength, NumOfColumns, GridOriginal, [X | Xs], Group, Path, ResAux, ListaCaminos, ListaCaminosAux)
	);
		adjacentIndexesList2(CurrentIndex, GridLength, NumOfColumns, GridOriginal, Xs, Group, Path, ResAux, ListaCaminos, ListaCaminosAux)
	), 
    append(Group, ResAux, Res),
    append([Path], ListaCaminosAux, ListaCaminosRes).


adjacentIndexesListAux2(XCheck, CurrentIndex, GridLength, NumOfColumns, GridOriginal, [_X | Xs], Group, Path, ResAux, ListaCaminos, ListaCaminosRes) :-
	((\+member(XCheck, Group),
	nth0(XCheck, GridOriginal, Elem1),
    nth0(CurrentIndex, GridOriginal, Value),
	(
		Elem1 =:= Value;
		length(Path, LengthPath),
		LengthPath > 1,
		Elem1 =:= Value * 2
	),
	adjacentIndexesList2(XCheck, GridLength, NumOfColumns, GridOriginal, [1,2,3,4,5,6,7,8], Group, Path, _GroupRes, ListaCaminos, ListaCaminosAux),
	adjacentIndexesList2(CurrentIndex, GridLength, NumOfColumns, GridOriginal, Xs, Group, Path, ResAux, ListaCaminosAux, ListaCaminosRes))
	;
	adjacentIndexesList2(CurrentIndex, GridLength, NumOfColumns, GridOriginal, Xs, Group, Path, ResAux, ListaCaminos, ListaCaminosRes)
	).


getBestPathInGridWithAdjacent(_, [], _, _, _, BestPath, BestPath).

getBestPathInGridWithAdjacent(CurrentIndex, [_X | Xs], CopyGrid, GridLength, NumOfColumns, PreviousBestPath, GetRes) :-
	adjacentIndexesList2(CurrentIndex, GridLength, NumOfColumns, CopyGrid, [1,2,3,4,5,6,7,8], [], [], _Res, [], Paths),
	getCorrectPaths(Paths, CopyGrid, GridLength, NumOfColumns, [], CorrectPaths),
	pathWithBestScore(CorrectPaths, CopyGrid, BestPath),
	pathWithBestScore([BestPath, PreviousBestPath], CopyGrid, NewBestPath),
	NewIndex is CurrentIndex + 1,
	getBestPathInGridWithAdjacent(NewIndex, Xs, CopyGrid, GridLength, NumOfColumns, NewBestPath, GetRes).


naf(X) :- X, !, fail.
naf(X).


getCorrectPaths([], _Grid, _GridLength, _NumOfColumns, CorrectPaths, CorrectPaths).

getCorrectPaths([P | Ps], Grid, GridLength, NumOfColumns, PCorrectPaths, CorrectPaths) :-
	lastBlockValue(Grid, P, Valor),
	checkMaxAdjacentEqual(P, Valor, Grid, GridLength, NumOfColumns),
	!,
	append(P, PCorrectPaths, NewCorrectPaths),
	getCorrectPaths(Ps, Grid, GridLength, NumOfColumns, NewCorrectPaths, CorrectPaths).

getCorrectPaths([P | Ps], Grid, GridLength, NumOfColumns, PCorrectPaths, CorrectPaths) :-
	getCorrectPaths(Ps, Grid, GridLength, NumOfColumns, PCorrectPaths, CorrectPaths).



checkMaxAdjacentEqual([X | Xs], Valor, Grid, GridLength, NumOfColumns) :-
	naf(checkMaxAdjacentEqualAux(X, Valor, Grid, GridLength, NumOfColumns)), 
	!, 
	checkMaxAdjacentEqual(Xs, Valor, Grid, GridLength, NumOfColumns).

checkMaxAdjacentEqual([X | Xs], Valor, Grid, GridLength, NumOfColumns) :-
	checkMaxAdjacentEqual(Xs, PathValue, Grid, GridLength, NumOfColumns).


checkMaxAdjacentEqualAux(X, PathValue, Grid, GridLength, NumOfColumns) :-
	CurrentIndex = X,
	(	checkAdjacentRight(CurrentIndex, NumOfColumns),
		X1 is CurrentIndex + 1,
		sameValue(PathValue, X1, Grid)
	);
	(	checkAdjacentLeft(CurrentIndex, NumOfColumns),
		X2 is CurrentIndex - 1,
		sameValue(PathValue, X2, Grid)
	);
	(	checkAdjacentTop(CurrentIndex, NumOfColumns),
		X3 is CurrentIndex - NumOfColumns,
		sameValue(PathValue, X3, Grid)
	);
	(	checkAdjacentBottom(CurrentIndex, GridLength, NumOfColumns),
		X4 is CurrentIndex + NumOfColumns,
		sameValue(PathValue, X4, Grid)
	);
	(	checkAdjacentBottomRight(CurrentIndex, GridLength, NumOfColumns),
		X5 is CurrentIndex + NumOfColumns + 1,
		sameValue(PathValue, X5, Grid)
	);
	(	checkAdjacentBottomLeft(CurrentIndex, GridLength, NumOfColumns),
		X6 is CurrentIndex + NumOfColumns - 1,
		sameValue(PathValue, X6, Grid)
	); 
	(	checkAdjacentTopRight(CurrentIndex, NumOfColumns),
		X7 is CurrentIndex - NumOfColumns + 1,
		sameValue(PathValue, X7, Grid)
	);
	(	checkAdjacentTopLeft(CurrentIndex, NumOfColumns),
		X8 is CurrentIndex - NumOfColumns - 1,
		sameValue(PathValue, X8, Grid)
	).

sameValue(PathValue, Index, Grid) :-
	nth0(Index, Grid, Value),
	PathValue =:= Value.





