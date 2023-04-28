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


/*
	RGrids es la lista de grillas representando el efecto, en etapas, de eliminar todos los grupos,
	poner los bloques correspondientes a cada grupo y aplicar la gravedad 

*/

booster(Grid, NumOfColumns, RGrids) :-
	getGroupList(Grid, Grid, NumOfColumns, 0, [], GroupList),
	flatten(GroupList, GroupListFlatted),
	newBlocksList(Grid, GroupList, [], NewBlocksList),
	emptyPathGrid(Grid, GroupListFlatted, 0, EmptyBoosterGrid),

	emptyBoosterGridWithBlocks(Grid, GroupList, NewBlocksList, EmptyBoosterGridWithBlocks),
	gridWithGravityApplied(EmptyBoosterGridWithBlocks, NumOfColumns, GridWithGravity),
	RGrids = [EmptyBoosterGrid, EmptyBoosterGridWithBlocks, GridWithGravity].



/*
	Devuelve la grilla con los bloques generados por cada grupo eliminado
*/
emptyBoosterGridWithBlocks(Grid, [], [], Grid).


emptyBoosterGridWithBlocks(Grid, [G | Gs], [S | Ss], EmptyBoosterGridWithBlocks) :-
	max_list(G, BlockIndex),
	replaceValueInGridPosition(Grid, BlockIndex, S, NewGrid),
	emptyBoosterGridWithBlocks(NewGrid, Gs, Ss, EmptyBoosterGridWithBlocks).

/*
	Devuelve una lista con el valor de los bloques a generar correspondientes a cada grupo
*/
newBlocksList(Grid, [], NewBlocksList, NewBlocksList).


newBlocksList(Grid, [G | Gs], NewBlocksList, Res) :-
	sumOfValuesInPath(Grid, G, SumOfPathValue),
	smallerPow2GreaterOrEqualThan(SumOfPathValue, NewBlock).
	append(NewBlock, NewBlocksList, NewBlocksListAux),
	newBlocksList(Grid, Gs, NewBlocksListAux, Res).


/*
	Devuelve la lista de grupos adyacentes en la grilla. Cada grupo es una lista conformada por los indices de las posiciones
*/

getGroupList([], _GridOriginal, _NumOfColumns, _CurrentPosition, GroupList, GroupList).

getGroupList(Grid, GridOriginal, NumOfColumns, CurrentPosition, GroupList, Res) :-
	Grid = [N | Ns],
    length(GridOriginal, GridLength),
	NewPosition is CurrentPosition + 1,
	flatten(GroupList, GroupListFlatted),
	(member(CurrentPosition, GroupListFlatted) ->
		getGroupList(Ns, GridOriginal, NumOfColumns, NewPosition, GroupList, ResAux) 	
	;
		calcularListaAdyacentes(CurrentPosition, GridLength, NumOfColumns, GridOriginal, [1,2,3,4,5,6,7,8], N, [], Group),
		length(Group, Length), 
		(Length > 1 -> 
			append([Group], GroupList, GroupListAux),
			getGroupList(Ns, GridOriginal, NumOfColumns, NewPosition, GroupListAux, ResAux)
           
        ;	
			getGroupList(Ns, GridOriginal, NumOfColumns, NewPosition, GroupList, ResAux)
		)
		
		
	),
    Res = ResAux.
	
	



calcularListaAdyacentes(_CurrentPosition, _GridLength, _NumOfColumns, _GridOriginal, [], _Valor, _Group, []).

	

calcularListaAdyacentes(CurrentPosition, GridLength, NumOfColumns, GridOriginal, [X | Xs], Valor, Group, Res) :-
	X =:= 1, 
    append([CurrentPosition], Group, GroupAux),
    
           ((checkAdjacentRight(CurrentPosition, NumOfColumns),
             X1 is CurrentPosition + 1,
              (
              	(\+member(X1, GroupAux),
                nth0(X1, GridOriginal, Elem1), 
                Elem1 =:= Valor, 
                calcularListaAdyacentes(X1, GridLength, NumOfColumns, GridOriginal, [1,2,3,4,5,6,7,8], Valor, GroupAux, ResAux));
             
              	calcularListaAdyacentes(CurrentPosition, GridLength, NumOfColumns, GridOriginal, Xs, Valor, GroupAux, ResAux)

              )
            );
                 calcularListaAdyacentes(CurrentPosition, GridLength, NumOfColumns, GridOriginal, Xs, Valor, GroupAux, ResAux)
               	 
            ),
    Res = ResAux.

    

calcularListaAdyacentes(CurrentPosition, GridLength, NumOfColumns, GridOriginal, [X | Xs], Valor, Group, Res) :-
	X =:= 2,
           ((checkAdjacentLeft(CurrentPosition, NumOfColumns),
             X2 is CurrentPosition - 1,
              (
              	(\+member(X2, Group),
                nth0(X2, GridOriginal, Elem1), 
                Elem1 =:= Valor, 
                calcularListaAdyacentes(X2, GridLength, NumOfColumns, GridOriginal, [1,2,3,4,5,6,7,8], Valor, Group, ResAux));
              
              	calcularListaAdyacentes(CurrentPosition, GridLength, NumOfColumns, GridOriginal, Xs, Valor, Group, ResAux)
              )
            );
                 calcularListaAdyacentes(CurrentPosition, GridLength, NumOfColumns, GridOriginal, Xs, Valor, Group, ResAux)
            ),
    Res = ResAux.

calcularListaAdyacentes(CurrentPosition, GridLength, NumOfColumns, GridOriginal, [X | Xs], Valor, Group, Res) :-
   	X =:= 3,
           ((checkAdjacentTop(CurrentPosition, NumOfColumns),
             X3 is CurrentPosition - NumOfColumns,
              (
              	(\+member(X3, Group),
                nth0(X3, GridOriginal, Elem1), 
                Elem1 =:= Valor, 
                calcularListaAdyacentes(X3, GridLength, NumOfColumns, GridOriginal, [1,2,3,4,5,6,7,8], Valor, Group, ResAux));
              
              	calcularListaAdyacentes(CurrentPosition, GridLength, NumOfColumns, GridOriginal, Xs, Valor, Group, ResAux)
              )
            );
                 calcularListaAdyacentes(CurrentPosition, GridLength, NumOfColumns, GridOriginal, Xs, Valor, Group, ResAux)
            ),
    Res = ResAux.

calcularListaAdyacentes(CurrentPosition, GridLength, NumOfColumns, GridOriginal, [X | Xs], Valor, Group, Res) :-
    X =:= 4,
           ((checkAdjacentBottom(CurrentPosition, GridLength, NumOfColumns),
             X4 is CurrentPosition + NumOfColumns,
              (
              	(\+member(X4, Group),
                nth0(X4, GridOriginal, Elem1), 
                Elem1 =:= Valor, 
                calcularListaAdyacentes(X4, GridLength, NumOfColumns, GridOriginal, [1,2,3,4,5,6,7,8], Valor, Group, ResAux));
              
              	calcularListaAdyacentes(CurrentPosition, GridLength, NumOfColumns, GridOriginal, Xs, Valor, Group, ResAux)
              )
            );
                 calcularListaAdyacentes(CurrentPosition, GridLength, NumOfColumns, GridOriginal, Xs, Valor, Group, ResAux)
            ),
    Res = ResAux.


calcularListaAdyacentes(CurrentPosition, GridLength, NumOfColumns, GridOriginal, [X | Xs], Valor, Group, Res) :-
    X =:= 5, 
           ((checkAdjacentBottomRight(CurrentPosition, GridLength, NumOfColumns),
             X5 is CurrentPosition + NumOfColumns + 1,
              (
              	(\+member(X5, Group),
                nth0(X5, GridOriginal, Elem1), 
                Elem1 =:= Valor, 
                calcularListaAdyacentes(X5, GridLength, NumOfColumns, GridOriginal, [1,2,3,4,5,6,7,8], Valor, Group, ResAux));
              
              	calcularListaAdyacentes(CurrentPosition, GridLength, NumOfColumns, GridOriginal, Xs, Valor, Group, ResAux)
              )
            );
                 calcularListaAdyacentes(CurrentPosition, GridLength, NumOfColumns, GridOriginal, Xs, Valor, Group, ResAux)
            ),
    Res = ResAux.

calcularListaAdyacentes(CurrentPosition, GridLength, NumOfColumns, GridOriginal, [X | Xs], Valor, Group, Res) :-
    X =:= 6, 
           ((checkAdjacentBottomLeft(CurrentPosition, GridLength, NumOfColumns),
             X6 is CurrentPosition + NumOfColumns - 1,
              (
              	(\+member(X6, Group),
                nth0(X6, GridOriginal, Elem1), 
                Elem1 =:= Valor, 
                calcularListaAdyacentes(X6, GridLength, NumOfColumns, GridOriginal, [1,2,3,4,5,6,7,8], Valor, Group, ResAux));
              
              	calcularListaAdyacentes(CurrentPosition, GridLength, NumOfColumns, GridOriginal, Xs, Valor, Group, ResAux)
              )
            );
                 calcularListaAdyacentes(CurrentPosition, GridLength, NumOfColumns, GridOriginal, Xs, Valor, Group, ResAux)
            ), 
    Res = ResAux.
    
calcularListaAdyacentes(CurrentPosition, GridLength, NumOfColumns, GridOriginal, [X | Xs], Valor, Group, Res) :-
    X =:= 7,
           ((checkAdjacentTopRight(CurrentPosition, NumOfColumns),
             X7 is CurrentPosition - NumOfColumns + 1,
              (
              	(\+member(X7, Group),
                nth0(X7, GridOriginal, Elem1), 
                Elem1 =:= Valor, 
                calcularListaAdyacentes(X7, GridLength, NumOfColumns, GridOriginal, [1,2,3,4,5,6,7,8], Valor, Group, ResAux));
              
              	calcularListaAdyacentes(CurrentPosition, GridLength, NumOfColumns, GridOriginal, Xs, Valor, Group, ResAux)
              )
            );
                 calcularListaAdyacentes(CurrentPosition, GridLength, NumOfColumns, GridOriginal, Xs, Valor, Group, ResAux)
            ),
   Res = ResAux.

calcularListaAdyacentes(CurrentPosition, GridLength, NumOfColumns, GridOriginal, [X | Xs], Valor, Group, Res) :-
    X =:= 8,
           ((checkAdjacentTopLeft(CurrentPosition, NumOfColumns),
             X8 is CurrentPosition - NumOfColumns - 1,
              (
              	(\+member(X8, Group),
                nth0(X8, GridOriginal, Elem1), 
                Elem1 =:= Valor, 
                calcularListaAdyacentes(X8, GridLength, NumOfColumns, GridOriginal, [1,2,3,4,5,6,7,8], Valor, Group, ResAux));
              
              	calcularListaAdyacentes(CurrentPosition, GridLength, NumOfColumns, GridOriginal, Xs, Valor, Group, ResAux)
              )
            );
                 calcularListaAdyacentes(CurrentPosition, GridLength, NumOfColumns, GridOriginal, Xs, Valor, Group, ResAux)
            ), 
    append(Group, ResAux, Res).



/* Booster colapsar iguales */

checkAdjacentRight(CurrentPosition, NumOfColumns) :-
	RightPosition is CurrentPosition + 1,
	RightPosition mod NumOfColumns =\= 0.

checkAdjacentLeft(CurrentPosition, NumOfColumns) :-
	CurrentPosition mod NumOfColumns =\= 0.

checkAdjacentTop(CurrentPosition, NumOfColumns) :-
	CurrentPosition >= NumOfColumns.

checkAdjacentBottom(CurrentPosition, GridLength, NumOfColumns) :-
	BottomPosition is CurrentPosition + NumOfColumns,
	BottomPosition < GridLength.

checkAdjacentBottomRight(CurrentPosition, GridLength, NumOfColumns) :-
	checkAdjacentBottom(CurrentPosition, GridLength, NumOfColumns),
	checkAdjacentRight(CurrentPosition, NumOfColumns).

checkAdjacentBottomLeft(CurrentPosition, GridLength, NumOfColumns) :-
	checkAdjacentBottom(CurrentPosition, GridLength, NumOfColumns),
	checkAdjacentLeft(CurrentPosition, NumOfColumns).

checkAdjacentTopRight(CurrentPosition, NumOfColumns) :-
	checkAdjacentTop(CurrentPosition, NumOfColumns),
	checkAdjacentRight(CurrentPosition, NumOfColumns).

checkAdjacentTopLeft(CurrentPosition, NumOfColumns) :-
	checkAdjacentTop(CurrentPosition, NumOfColumns),
	checkAdjacentLeft(CurrentPosition, NumOfColumns).
