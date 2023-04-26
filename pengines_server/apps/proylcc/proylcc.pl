:- module(proylcc, 
	[  
		join/4
	]).


/**
 * join(+Grid, +NumOfColumns, +Path, -RGrids) 
 * RGrids es la lista de grillas representando el efecto, en etapas, de combinar las celdas del camino Path
 * en la grilla Grid, con número de columnas NumOfColumns. El número 0 representa que la celda está vacía. 
 */ 

 :- module(proylcc, 
	[  
		join/4
	]).

join(Grid, NumOfColumns, Path, RGrids) :-
	Grid = [_N | _Ns],	% La implementación actual es simplemente a modo de muestra, y no tiene sentido, debe reepmplazarl	% por una implementación válida.
	positionPath(Path, NumOfColumns, PositionPath),
	lastBlockValue(Grid, PositionPath, LastBlockValue),
	emptyPathGrid(Grid, PositionPath, 0, EmptyPathGrid),
	last(PositionPath, Last),
	setLastBlock(EmptyPathGrid, 0, Last, LastBlockValue, EmptyPathGridWithLastBlock),
	gridWithGravityApplied(EmptyPathGridWithLastBlock, NumOfColumns, GridWithGravity),
	RGrids = [EmptyPathGrid, EmptyPathGridWithLastBlock, GridWithGravity]. % , [_N2 | Ns]].


positionPath(Path, NumOfColumns, PositionPath) :-
    positionPathAux(Path, NumOfColumns, [], PositionPath).

positionPathAux([], _, PositionPath, Result) :-
    Result = PositionPath.
positionPathAux([[N | Ns] | T], NumOfColumns, PositionPath, Result) :-
    Position is N * NumOfColumns +  Ns,
    append(PositionPath, [Position] , AppendedList),
    positionPathAux(T, NumOfColumns, AppendedList, Result).


/*
	Recorre la grilla y verifica si la posicion actual de la grilla es miembtro de la lista de posiciones del path
	En caso de serlo, se modificara el valor del elemento de la grilla a 0, sino este no cambiara
*/

emptyPathGrid([_N], PositionPath, PosActual, EmptyGrid) :- 
	member(PosActual, PositionPath),
	EmptyGrid = [0].

emptyPathGrid([N], PositionPath, PosActual, EmptyGrid) :- 
	\+member(PosActual, PositionPath),
	EmptyGrid = [N].


emptyPathGrid([_N | Ns], PositionPath, PosActual, EmptyGrid) :- 
    NextPos is PosActual + 1,
	member(PosActual, PositionPath),
	emptyPathGrid(Ns, PositionPath, NextPos, EmptyGridAux),
	append([0], EmptyGridAux, EmptyGrid).

emptyPathGrid([N | Ns], PositionPath, PosActual, EmptyGrid) :- 
    NextPos is PosActual + 1,
	\+member(PosActual, PositionPath),
	emptyPathGrid(Ns, PositionPath, NextPos, EmptyGridAux),
	append([N], EmptyGridAux, EmptyGrid).



/* 
	Recorre la lista hasta ubicar la posicion de la grilla correspondiente a la ultima posicion del path,
	reemplazando su valor por la potencia de dos mas cercana a la suma de los elementos del path.
*/
setLastBlock([_N | Ns], Inicio, Fin, Valor, [Valor | Ns]) :-
	Inicio =:= Fin.

setLastBlock([N | Ns], Inicio, Fin, Valor, ModGrid) :-
	Inicio =\= Fin,
    NewInicio is Inicio + 1,
	setLastBlock(Ns, NewInicio, Fin, Valor, ModGridAux),
	append([N], ModGridAux, ModGrid).


/*
	Devuelve la suma de valores de la grilla en un determinado camino 
*/

sumOfValuesInPath(Grid, PositionPath, Result) :-
	sumOfValuesInPathAux(Grid, PositionPath, 0, 0, Result).

sumOfValuesInPathAux([], _, _, SummedValues, Result) :-
	Result = SummedValues.

sumOfValuesInPathAux([G | Gs], PositionPath, CurrentGridPosition, SummedValues, Result) :-
	NextGridPosition is CurrentGridPosition + 1,
	(member(CurrentGridPosition, PositionPath) ->
		SummedValuesAux is SummedValues + G,
		sumOfValuesInPathAux(Gs, PositionPath, NextGridPosition, SummedValuesAux, Result)
	;
		sumOfValuesInPathAux(Gs, PositionPath, NextGridPosition, SummedValues, Result)
	).
	
/*
	Devuelve el valor que debería tener el bloque resultante
*/

lastBlockValue(Grid, PositionPath, LastBlockValue) :-
	sumOfValuesInPath(Grid, PositionPath, SumOfPathValue),
	smallerPow2GreaterOrEqualThan(SumOfPathValue, LastBlockValue).

/*
	Calcula la potencia de dos mas cercana o igual a un determinado numero
*/

smallerPow2GreaterOrEqualThan(Num, Resultado) :-
	Log2Num is floor(log(Num)/log(2)),
	pow(2, Log2Num, ResPow),
	(
    	ResPow =:= Num,
		Resultado = Num
    ;
    	Log2NumAux is Log2Num + 1,
    	pow(2, Log2NumAux, Resultado)
    ).

/* Implementaciones de gravedad */

/* 
	Devuelve una grilla luego de aplicar gravedad
*/
gridWithGravityApplied(GridWithEmptyPath, NumOfColumns, Result) :-
	reverse(GridWithEmptyPath, ReversedGrid),
	gridWithGravityAppliedAux(ReversedGrid, NumOfColumns, 0, [], ResultAux1),
    reverse(ResultAux1, ResultAux2),
	replaceZeros(ResultAux2, Result).

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

/* 
	Devuelve la posición dentro de la lista del bloque no vacío más cercano dentro de la misma columna
	Si no existe un bloque por encima no vacío, devuelve 0
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

/* 
	Devuelve una lista que reemplaza el valor en cierta posición por un nuevo valor
*/
replaceValueInGridPosition(List, Position, NewValue, Result) :-
	nth0(Position, List, _, ListRemainder),
	nth0(Position, Result, NewValue, ListRemainder).


replaceZeros([N], Result) :-
	((N =:= 0) ->
		generateRandomBlock(X),
		Result = [X]
	;
		Result = [N]
	).

replaceZeros([N | Ns], Result) :-
	replaceZeros(Ns, ResultAux),
	((N =:= 0) ->
		generateRandomBlock(X),
		append([X], ResultAux, Result)
	;
		append([N], ResultAux, Result)
	).

/*
	Genera un bloque aleatorio
*/

generateRandomBlock(Num) :-
	random(1, 7, X),
	pow(2, X, Num).