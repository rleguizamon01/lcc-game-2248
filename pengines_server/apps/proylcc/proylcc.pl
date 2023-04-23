:- module(proylcc, 
	[  
		join/4
	]).


/**
 * join(+Grid, +NumOfColumns, +Path, -RGrids) 
 * RGrids es la lista de grillas representando el efecto, en etapas, de combinar las celdas del camino Path
 * en la grilla Grid, con número de columnas NumOfColumns. El número 0 representa que la celda está vacía. 
 */ 

join(Grid, _NumOfColumns, _Path, RGrids):-
	Grid = [N | Ns],	% La implementación actual es simplemente a modo de muestra, y no tiene sentido, debe reepmplazarla
	N2 is N * 2,		% por una implementación válida.
	positionList(Path, _NumOfColumns, PositionPath),
	emptyGrid2(Grid, PositionPath, 0, EmptyGrid),
	last(PositionPath, Last),
	setNewBlock(Grid, 0, Last, 77, OldGrid),
	RGrids = [OldGrid, [N2 | Ns]].


positionList(Path, NumOfColumns, PositionPath) :-
    positionListAux(Path, NumOfColumns, [], PositionPath).

positionListAux([], _, PositionPath, Result) :-
    Result = PositionPath.
positionListAux([N | T], NumOfColumns, PositionPath, Result) :-
    N = [NH | NT],
    Position is NH * NumOfColumns +  NT,
    append(PositionPath, [Position] , AppendedList),
    positionListAux(T, NumOfColumns, AppendedList, Result).


/*
	Recorre la grilla y verifica si la posicion actual de la grilla es miembtro de la lista de posiciones del path
	En caso de serlo, se modificara el valor del elemento de la grilla a 0, sino este no cambiara
*/

emptyGrid2([N], PositionPath, PosActual, EmptyGrid) :- 
	member(PosActual, PositionPath),
	EmptyGrid = [0].

emptyGrid2([N], PositionPath, PosActual, EmptyGrid) :- 
	\+member(PosActual, PositionPath),
	EmptyGrid = [N].


emptyGrid2([N | Ns], PositionPath, PosActual, EmptyGrid) :- 
	(member(PosActual, PositionPath),
	emptyGrid2(Ns, PositionPath, PosActual + 1, EmptyGridAux),
	append([0], EmptyGridAux, EmptyGrid));
	emptyGrid2(Ns, PositionPath, PosActual + 1, EmptyGridAux),
	append([N], EmptyGridAux, EmptyGrid).


/* 
	Recorre la lista hasta ubicar la posicion de la grilla correspondiente a la ultima posicion del path,
	reemplazando su valor por la potencia de dos mas cercana a la suma de los elementos del path.
*/
setNewBlock([N | Ns], Inicio, Fin, Valor, [Valor | Ns]) :-
	Inicio =:= Fin.

setNewBlock([N | Ns], Inicio, Fin, Valor, ModGrid) :-
	Inicio =\= Fin,
	setNewBlock(Ns, Inicio + 1, Fin, Valor, ModGridAux),
	append(N, ModGridAux, ModGrid).

	







/*
	Implementaciones viejas de emptyGrid
*/

emptyGrid(Grid, _NumOfColumns, Path, EmptyGrid) :-
	Path = [[N1 | N1s] | [N2s | N2]],
	Position = N1 * _NumOfColumns +  N1s;
	recorrerYReemplazar(Grid, 0, Position, ModGrid);
	emptyGrid(ModGrid, _NumOfColumns, [N2s | N2], EmptyGrid).


recorrerYReemplazar(Grid, Inicio, Fin, ModGrid) :-
	Inicio = Fin, 
	Grid = [N | Ns],
	ModGrid = [0 | Ns];
	recorrerYReemplazar(Grid, Inicio + 1, Fin, ModGrid).
 



