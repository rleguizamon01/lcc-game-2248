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
	RGrids = [EmptyPathGridWithLastBlock, GridWithGravity]. % , [_N2 | Ns]].


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

/* Implementaciones de gravedad 
   Revierte la lista
   Si hay 0 en la lista, la recorre
   Si el valor actual es 0, aplica gravedad en esa columna una única vez
		Si CurrentPosition + NumOfColumns < GridLength
			AboveBlockValue = Grid[CurrentPosition+NumOfColumns]
			Si AboveBlockValue es 0
				Recursivo! CurrentPosition+NumOfColumns
			Sino
				Return = AboveBlackValue
		Sino
			Return = 0
			
*/

gridWithGravityApplied(GridWithEmptyPath, NumOfColumns, Result) :-
	reverse(GridWithEmptyPath, ReversedGrid),
	gridWithGravityAppliedAux(ReversedGrid, NumOfColumns, 0, [], ResultAux),
    reverse(ResultAux, Result).

gridWithGravityAppliedAux([], _, _, GridWithGravity, Result) :-
	Result = GridWithGravity.

gridWithGravityAppliedAux([CurrentBlock | T], NumOfColumns, CurrentPosition, GridWithGravity, Result) :-
	NextPosition is CurrentPosition + 1,
	% Si el bloque actual está vacío
	(CurrentBlock =:= 0 ->
		closestNotEmptyAboveBlock(T, NumOfColumns, AboveBlock),
        append(GridWithGravity, [AboveBlock], GridWithGravityAppended),
        (AboveBlock =\= 0 ->
        	closestNotEmptyAboveBlockToZero([CurrentBlock | T], NumOfColumns, NumOfColumns, GridWithGravityWithZeroAbove),
            GridWithGravityWithZeroAbove = [_ | TAux],
            gridWithGravityAppliedAux(TAux, NumOfColumns, NextPosition, GridWithGravityAppended, Result)
        
        ;   
        	gridWithGravityAppliedAux(T, NumOfColumns, NextPosition, GridWithGravityAppended, Result)
        )
	;
		append(GridWithGravity, [CurrentBlock], GridWithGravityAppended),
		gridWithGravityAppliedAux(T, NumOfColumns, NextPosition, GridWithGravityAppended, Result)
	).

closestNotEmptyAboveBlockToZero(Grid, Position, NumOfColumns, Result) :-
	nth0(Position, Grid, Value, _),
	(Value =:= 0 ->
		DoublePosition is Position + NumOfColumns,
		closestNotEmptyAboveBlockToZero(Grid, DoublePosition, NumOfColumns, Result)
	;
		replaceValueInGrid(Grid, Position, 0, Result)
	).
	
replaceValueInGrid(List, Position, NewValue, Result) :-
	nth0(Position, List, _, ListRemainder),
	nth0(Position, Result, NewValue, ListRemainder).

closestNotEmptyAboveBlock(Grid, NumOfColumns, Result) :-
	closestNotEmptyAboveBlockAux(Grid, NumOfColumns, 1, Result).

closestNotEmptyAboveBlockAux([], _, _, Result) :-
	Result = 0.

closestNotEmptyAboveBlockAux([CurrentBlock | T], NumOfColumns, CurrentPosition, Result) :-
	% Si la posición actual pertenece a la columna correcta
	(((CurrentPosition mod NumOfColumns) =\= 0) ->
		NextPosition is CurrentPosition + 1,
		closestNotEmptyAboveBlockAux(T, NumOfColumns, NextPosition, Result)
	;
		% Si el bloque actual no está vacío
		(CurrentBlock =\= 0 ->
			Result = CurrentBlock
		;
			closestNotEmptyAboveBlockAux(T, NumOfColumns, 1, Result)
		)
	).