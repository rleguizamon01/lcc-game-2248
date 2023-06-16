import React, { useEffect, useState } from 'react';
import PengineClient from './PengineClient';
import Board from './Board';
import { getPositionFromIndex, joinResult } from './util';
import Square from './Square';

let pengine;

function Game() {

  // State
  const [grid, setGrid] = useState(null);
  const [numOfColumns, setNumOfColumns] = useState(null);
  const [score, setScore] = useState(0);
  const [path, setPath] = useState([]);
  const [waiting, setWaiting] = useState(false);
  const [pathScore, setPathScore] = useState(0);

  useEffect(() => {
    // This is executed just once, after the first render.
    PengineClient.init(onServerReady);
  }, []);

  /**
   * Called when the server was successfully initialized
   */
  function onServerReady(instance) {
    pengine = instance;
    const queryS = 'init(Grid, NumOfColumns)';
    pengine.query(queryS, (success, response) => {
      if (success) {
        setGrid(response['Grid']);
        setNumOfColumns(response['NumOfColumns']);
      }
    });
  }

  /**
   * Called while the user is drawing a path in the grid, each time the path changes.
   */
  function onPathChange(newPath) {
    console.log(newPath);
    // No effect if waiting.
    if (waiting) {
      return;
    }
    setPath(newPath);
    setPathScore(joinResult(newPath, grid, numOfColumns));
    console.log(JSON.stringify(newPath));
  }

  /**
   * Called when the user finished drawing a path in the grid.
   */
  function onPathDone() {
    const gridS = JSON.stringify(grid);
    console.log(gridS);
    const pathS = JSON.stringify(path);
    let valueNewBlock = joinResult(path, grid, numOfColumns);
    console.log(valueNewBlock);


    const queryS = "join(" + gridS + "," + numOfColumns + "," + pathS + ", RGrids)";
    setWaiting(true);
  


    pengine.query(queryS, (success, response) => {
      if (success) {
        setScore(score + joinResult(path, grid, numOfColumns));
        setPathScore(0);
        setPath([]);
        
        animateEffect(response['RGrids']);
        
      } else {
        setWaiting(false);
      }
    });
  }

  /**
   * Displays each grid of the sequence as the current grid in 1sec intervals.
   * @param {number[][]} rGrids a sequence of grids.
   */
  function animateEffect(rGrids) {
    setGrid(rGrids[0]);
    const restRGrids = rGrids.slice(1);
    if (restRGrids.length > 0) {
      setTimeout(() => {
        animateEffect(restRGrids);
      }, 300);
    } else {
      setWaiting(false);
    }
  }

  /**
   * Called when the user clicks the booster button 1
   */
  function onCallBooster1() {
    if (waiting || path.length > 1)
      return;

    const gridS = JSON.stringify(grid);
    const queryS = "booster(" + gridS + ", " + numOfColumns + ", RGrids)";
    setWaiting(true);
    pengine.query(queryS, (success, response) => {
      if (success) {
        animateEffect(response['RGrids']);
      } else {
        setWaiting(false);
      }
    });
  }

  /**
   * Called when the user clicks the booster button 2
   */
  function onCallBooster2() {
    if (waiting)
      return;

    const gridS = JSON.stringify(grid);
    const queryS = "booster2(" + gridS + ", " + numOfColumns + ", Res)";
    setWaiting(true);
    pengine.query(queryS, (success, response) => {
      setWaiting(false);

      if (success) {
        let bestPath = response['Res'];
        const bestPositionPath = bestPath.map(index =>
          getPositionFromIndex(index, numOfColumns)
        );

        for (let i = 1; i <= bestPositionPath.length; i++) {
          const subArray = bestPositionPath.slice(0, i);
          onPathChange(subArray);
        }
      }
    });
  }

  /**
   * Called when the user clicks the booster button 3
   */
  function onCallBooster3() {
    if (waiting)
      return;

    const gridS = JSON.stringify(grid);
    const queryS = "booster3(" + gridS + ", " + numOfColumns + ", Res)";
    setWaiting(true);
    pengine.query(queryS, (success, response) => {
      setWaiting(false);

      if (success) {
        let bestPath = response['Res'];
        const bestPositionPath = bestPath.map(index =>
          getPositionFromIndex(index, numOfColumns)
        );

        for (let i = 1; i <= bestPositionPath.length; i++) {
          const subArray = bestPositionPath.slice(0, i);
          onPathChange(subArray);
        }
      }
    });
  }

  if (grid === null) {
    return null;
  }
  return (
    <div className="game">
      <div className="header">
        {path.length <= 1 
          ? <div className="score">{score}</div>
          : <Square value={pathScore} className={'scoreSquare'}/>
        }
      </div>
      <Board
        grid={grid}
        numOfColumns={numOfColumns}
        path={path}
        onPathChange={onPathChange}
        onDone={onPathDone}
      /> 
      <div>
        <img
          className="booster"
          src="booster1.png"
          alt="Botón Booster"
          onClick={onCallBooster1}
        />
        <img
          className="booster"
          src="booster2.png"
          alt="Botón Booster Movida Máxima"
          onClick={onCallBooster2}
        />
        <img
          className="booster"
          src="booster3.png"
          alt="Botón Booster Máximos Adyacentes Iguales"
          onClick={onCallBooster3}
        />
      </div>
    </div>
  );
}

export default Game;