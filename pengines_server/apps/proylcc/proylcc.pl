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
	gridWithRandomValues(GridWithGravity, GridWithRandomValues),
	RGrids = [GridWithEmptyPath, GridWithLastBlock, GridWithGravity, GridWithRandomValues].

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

gridWithRandomValues(Grid, Result) :-
	replaceZerosWithRandomValue(Grid, Result).
/**
 * gridWithGravity(+GridWithEmptyPath, +NumOfColumns, -Result)
 * Devuelve una grilla luego de aplicar gravedad
 */
gridWithGravity(GridWithEmptyPath, NumOfColumns, Result) :-
	reverse(GridWithEmptyPath, ReversedGrid),
	gridWithGravityAppliedAux(ReversedGrid, NumOfColumns, 0, [], ResultAux1),
    reverse(ResultAux1, Result).
	

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
	gridWithRandomValues(GridWithGravity, GridWithRandomValues),
	RGrids = [GridWithEmptyPath, GridWithBoosterBlocks, GridWithGravity, GridWithRandomValues].

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

/*
*
* Entrega 2
*
*/

/*
* booster2(+Grid, +NumOfColumns, -Res)
* Res es el camino de la grilla con mayor valor
*/
booster2(Grid, NumOfColumns, Res) :-
	length(Grid, GridLength),
	getBestPathInGrid(0, Grid, Grid, GridLength, NumOfColumns, [], BestPath),
	reverse(BestPath, Res).

/*
* getBestPathInGrid(+CurrentIndex, +Grid, +Grid, +GridLength, +NumOfColumns, +PreviousBestPath, -BestPath)
* Devuelve el camino con mayor valor de la grilla
*/
getBestPathInGrid(_CurrentIndex, [], _CopyGrid, _GridLength, _NumOfColumns, PreviousBestPath, PreviousBestPath).

getBestPathInGrid(CurrentIndex, [_X | Xs], CopyGrid, GridLength, NumOfColumns, PreviousBestPath, GetRes) :-
	allPathsFromIndex(CurrentIndex, GridLength, NumOfColumns, CopyGrid, [], [], Paths),
	pathWithBestScore(Paths, CopyGrid, BestPath),
	pathWithBestScore([BestPath, PreviousBestPath], CopyGrid, NewBestPath),
	NewIndex is CurrentIndex + 1,
	getBestPathInGrid(NewIndex, Xs, CopyGrid, GridLength, NumOfColumns, NewBestPath, GetRes).

/*
* pathWithBestScore(+Paths, +Grid, -Res)
* Dado un grupo de caminos y la grilla devuelve el camino que mas valor tiene entre ellos
*/
pathWithBestScore(Paths, Grid, Res) :-
	pathWithBestScoreAux(Paths, Grid, [], 0, Res).

pathWithBestScoreAux([], _, BestPath, _, BestPath).
pathWithBestScoreAux([P | Ps], Grid, BestPath, BestPathScore, Res) :-
	length(P, LengthPath),
    LengthPath > 1,
	valuePath(P, Grid, [], ValuePath),
	sum_list(ValuePath, ValuePathSummed),
	(
		ValuePathSummed > BestPathScore,
		pathWithBestScoreAux(Ps, Grid, P, ValuePathSummed, Res)
	;
		pathWithBestScoreAux(Ps, Grid, BestPath, BestPathScore, Res)
	).

/*
* valuePath(+IndexPath, +Grid, +ValuePath, -Res)
* Dado un camino de índices, devuelve una lista de sus valores
*/
valuePath([], _Grid, ValuePath, ValuePath).
valuePath([I | Is], Grid, ValuePath, Res) :-
	nth0(I, Grid, Value),
	append(ValuePath, [Value], ValuePathAppended),
	valuePath(Is, Grid, ValuePathAppended, Res).

/*
* allPathsFromIndex(+CurrentIndex, +GridLength, +NumOfColumns, +GridOriginal, )
* Dado un determinado indice, se obtienen todos los caminos posibles partiendo desde dicho indice
*/
allPathsFromIndex(CurrentIndex, GridLength, NumOfColumns, Grid, Path, Paths, Res) :-
    append([CurrentIndex], Path, PathAppended),
    append([PathAppended], Paths, PathsRes1),
	(	checkAdjacentRight(CurrentIndex, NumOfColumns),
		X1 is CurrentIndex + 1,
		adjacentIndexesListAux2(X1, CurrentIndex, GridLength, NumOfColumns, Grid, PathAppended, PathsRes1, Res1)
	;	Res1 = PathsRes1
	),
	(	checkAdjacentLeft(CurrentIndex, NumOfColumns),
		X2 is CurrentIndex - 1,
		adjacentIndexesListAux2(X2, CurrentIndex, GridLength, NumOfColumns, Grid, PathAppended, Res1, Res2)
	;	Res2 = Res1
	),
	(	checkAdjacentTop(CurrentIndex, NumOfColumns),
		X3 is CurrentIndex - NumOfColumns,
		adjacentIndexesListAux2(X3, CurrentIndex, GridLength, NumOfColumns, Grid, PathAppended, Res2, Res3)
	;	Res3 = Res2
	),
	(	checkAdjacentBottom(CurrentIndex, GridLength, NumOfColumns),
		X4 is CurrentIndex + NumOfColumns,
		adjacentIndexesListAux2(X4, CurrentIndex, GridLength, NumOfColumns, Grid, PathAppended, Res3, Res4)
	;	Res4 = Res3
	),
	(	checkAdjacentBottomRight(CurrentIndex, GridLength, NumOfColumns),
		X5 is CurrentIndex + NumOfColumns + 1,
		adjacentIndexesListAux2(X5, CurrentIndex, GridLength, NumOfColumns, Grid, PathAppended, Res4, Res5)
	;	Res5 = Res4
	),
	(	checkAdjacentBottomLeft(CurrentIndex, GridLength, NumOfColumns),
		X6 is CurrentIndex + NumOfColumns - 1,
		adjacentIndexesListAux2(X6, CurrentIndex, GridLength, NumOfColumns, Grid, PathAppended, Res5, Res6)
	;	Res6 = Res5
	),
	(	checkAdjacentTopRight(CurrentIndex, NumOfColumns),
		X7 is CurrentIndex - NumOfColumns + 1,
		adjacentIndexesListAux2(X7, CurrentIndex, GridLength, NumOfColumns, Grid, PathAppended, Res6, Res7)
	;	Res7 = Res6
	),
	(	checkAdjacentTopLeft(CurrentIndex, NumOfColumns),
		X8 is CurrentIndex - NumOfColumns - 1,
		adjacentIndexesListAux2(X8, CurrentIndex, GridLength, NumOfColumns, Grid, PathAppended, Res7, Res)
	;	Res = Res7
	).

adjacentIndexesListAux2(NewIndex, CurrentIndex, GridLength, NumOfColumns, Grid, Path, PathsRes, Res) :-
	\+member(NewIndex, Path),
	nth0(NewIndex, Grid, NewIndexValue),
	nth0(CurrentIndex, Grid, CurrentIndexValue),
	(
		NewIndexValue =:= CurrentIndexValue;
        length(Path, LengthPath),
        LengthPath > 1,
		NewIndexValue =:= CurrentIndexValue * 2
	),
	allPathsFromIndex(NewIndex, GridLength, NumOfColumns, Grid, Path, PathsRes, Res).

/*
* booster3(+Grid, +NumOfColumns, -Res)
* Res devuelve el camino que cumple con la condición del segundo ejercicio
*/
booster3(Grid, NumOfColumns, Res) :-
	length(Grid, GridLength),
	getBestPathInGridWithAdjacent(0, Grid, Grid, GridLength, NumOfColumns, [], BestPath),
	reverse(BestPath, Res).

/*
* getBestPathInGridWithAdjacent(+CurrentIndex, +Grid, +CopyGrid, +GridLength, +NumOfColumns, +PreviousBestPath, -Res)
* Devuelve el mejor mejor camino que cumpla con la condición del segundo ejercicio
*/
getBestPathInGridWithAdjacent(_, [], _, _, _, BestPath, BestPath).

getBestPathInGridWithAdjacent(CurrentIndex, [_X | Xs], CopyGrid, GridLength, NumOfColumns, PreviousBestPath, GetRes) :-
	allPathsFromIndex(CurrentIndex, GridLength, NumOfColumns, CopyGrid, [], [], Paths),
	getCorrectPaths(Paths, CopyGrid, GridLength, NumOfColumns, [], CorrectPaths),
	pathWithBestScore(CorrectPaths, CopyGrid, BestPath),
	pathWithBestScore([BestPath, PreviousBestPath], CopyGrid, NewBestPath),
	NewIndex is CurrentIndex + 1,
	getBestPathInGridWithAdjacent(NewIndex, Xs, CopyGrid, GridLength, NumOfColumns, NewBestPath, GetRes).

/*
* checkAdjacentsSameValue(+CurrentIndex, +PathValue, +Grid, +GridLength, +NumOfColumns)
* Dado un determinado índice determina si el valor de alguno de sus adyacentes coincide con el valor
* asociado al indice
*/
checkAdjacentsSameValue(CurrentIndex, PathValue, Grid, GridLength, NumOfColumns) :-
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

/*
* sameValue(+PathValue, +Index, +Grid)
* Dada la grilla, un determinado valor y un índice determina si el valor asociado al índice es igual al pasado por 
* parámetro
*/
sameValue(PathValue, Index, Grid) :-
	nth0(Index, Grid, Value),
	PathValue =:= Value.

/*
* getCorrectPaths(+Paths, +Grid, +GridLength, +NumOfColumns, +CorrectPaths, Res)
* Recibe un grupo de caminos por parametro y devuelve aquellos que cumplen que su bloque generado, una vez aplicada la gravedad
* tiene como adyacente a un bloque con el mismo valor
*/
getCorrectPaths([], _Grid, _GridLength, _NumOfColumns, CorrectPaths, CorrectPaths).

getCorrectPaths([P | Ps], Grid, GridLength, NumOfColumns, PreviousCorrectPaths, CorrectPaths) :-
	length(P, PathLength),
	PathLength > 1,
	lastBlockValue(Grid, P, LastBlockValue),
	gridWithEmptyPath(Grid, P, 0, GridWithEmptyPath),
	P = [LastPathIndex | _Ls],
	replaceValueInGridIndex(GridWithEmptyPath, LastPathIndex, LastBlockValue, GridWithLastBlock),
	gridWithGravity(GridWithLastBlock, NumOfColumns, GridWithGravity),
	indexAfterGravity(P, LastPathIndex, NumOfColumns, NewIndexAfterGravity),
    checkAdjacentsSameValue(NewIndexAfterGravity, LastBlockValue, GridWithGravity, GridLength, NumOfColumns),
	!,
	append([P], PreviousCorrectPaths, NewCorrectPaths),
	getCorrectPaths(Ps, Grid, GridLength, NumOfColumns, NewCorrectPaths, CorrectPaths).

getCorrectPaths([_ | Ps], Grid, GridLength, NumOfColumns, PreviousCorrectPaths, CorrectPaths) :-
	getCorrectPaths(Ps, Grid, GridLength, NumOfColumns, PreviousCorrectPaths, CorrectPaths).

/*
* indexAfterGravity(+Path, +LastPathIndex, +NumOfColumns, -Res)
* Dado un determinado camino, el número de columnas y el índice de un bloque del camino 
* determina el nuevo índice de dicho bloque una vez aplicada la gravedad
*
*/
indexAfterGravity(Path, LastPathIndex, NumOfColumns, Res) :-
	indexAfterGravityAux(Path, LastPathIndex, LastPathIndex, NumOfColumns, Res). 
		
indexAfterGravityAux([], _LastPathIndex, NewIndex, _NumOfColumns, NewIndex).

indexAfterGravityAux([P | Ps], LastPathIndex, NewIndex, NumOfColumns, Res) :-
	sameColumn(P, LastPathIndex, NumOfColumns),
	P > LastPathIndex,
	NextIndex is NewIndex + NumOfColumns,
	indexAfterGravityAux(Ps, LastPathIndex, NextIndex, NumOfColumns, Res).

indexAfterGravityAux([_P | Ps], LastPathIndex, NewIndex, NumOfColumns, Res) :-
	indexAfterGravityAux(Ps, LastPathIndex, NewIndex, NumOfColumns, Res).

/*
* sameColumn(+Index1, +Index2, +NumOfColumns)
* Dado dos índices y el número de columnas determina si ambos índices corresponden a la misma columna
*/
sameColumn(Index1, Index2, NumOfColumns) :-
	DistanceBetweenIndexes is abs(Index1 - Index2),
	DistanceBetweenIndexes mod NumOfColumns =:= 0.