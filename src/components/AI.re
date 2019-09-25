open Shared;

type eval = (score, move);

let getAvailableSpots = board => {
  board
  |> List.mapi((rowIndex, row) =>
       row
       |> List.mapi((tileIndex, value) =>
            value == Empty
              ? {
                let id = string_of_int(rowIndex) ++ string_of_int(tileIndex);
                id;
              }
              : ""
          )
     )
  |> List.flatten
  |> List.filter(value => value != "");
};

let getBestMoveScore = (scoreMoves, player) => {
  let initialEval = player == Computer ? ((-1000), "") : (1000, "");
  scoreMoves->Belt.List.reduce(
    initialEval, ((bestScore, bestMove), (curScore, curMove)) =>
    switch (player) {
    | Computer =>
      curScore > bestScore ? (curScore, curMove) : (bestScore, bestMove)
    | Human =>
      curScore < bestScore ? (curScore, curMove) : (bestScore, bestMove)
    }
  );
};

let rec minimax = (board, player) => {
  let flattened = board |> List.flatten;
  let avaialableTiles = getAvailableSpots(board);
  let gameState = checkGameStatus(winTable, flattened, Turn(player));

  let getScores = moves => {
    moves
    |> List.map(move => {
         let updatedBoard = makeMove(board, move, gameState);
         let (score, _) = minimax(updatedBoard, oppositePlayer(player));
         //  Js.log2(score, move);
         (score, move);
       });
  };

  switch (gameState, avaialableTiles) {
  | (Won(Computer), _) => (10, "")
  | (Won(Human), _) => ((-10), "")
  | (Tie, []) => (0, "")
  | (Turn(_), moves) => moves->getScores->getBestMoveScore(player)
  };
};

let getBestMove = board => {
  let (score, move) = minimax(board, Computer);
  Js.log("---------Final Score--------");
  Js.log2(score, move);
  move;
};