type player =
  | Human
  | Computer;

type tile =
  | Empty
  | Marked(player);

type winner =
  | Cross
  | Circle
  | None;

type row = list(tile);
type board = list(row);

type gameStatus =
  | Turn(player)
  | Won(player)
  | Tie
  | None;

type score = int;
type move = string;

type gameScores = {
  cross: score,
  circle: score,
};

let winTable = [
  [0, 1, 2],
  [3, 4, 5],
  [6, 7, 8],
  [0, 3, 6],
  [1, 4, 7],
  [2, 5, 8],
  [0, 4, 8],
  [2, 4, 6],
];

let checkIfWon = (flattened, head) => {
  switch (
    List.nth(flattened, List.nth(head, 0)),
    List.nth(flattened, List.nth(head, 1)),
    List.nth(flattened, List.nth(head, 2)),
  ) {
  | (Marked(Human), Marked(Human), Marked(Human)) => Circle
  | (Marked(Computer), Marked(Computer), Marked(Computer)) => Cross
  | (_, _, _) => None
  };
};

let oppositePlayer = player => {
  switch (player) {
  | Human => Computer
  | Computer => Human
  };
};

let rec checkGameStatus = (winTable, flattenedBoard, gameStatus) => {
  let head = List.hd(winTable);
  let tail = List.tl(winTable);

  let isTied =
    flattenedBoard
    |> List.for_all(tile =>
         tile == Marked(Human) || tile == Marked(Computer)
       );

  switch (checkIfWon(flattenedBoard, head), tail, isTied) {
  | (Circle, _, _) => Won(Human)
  | (Cross, _, _) => Won(Computer)
  | (_, [], _) when gameStatus == Turn(Computer) => Turn(Human)
  | (_, [], _) when gameStatus == Turn(Human) => Turn(Computer)
  | (_, _, true) => Tie
  | _ => checkGameStatus(tail, flattenedBoard, gameStatus)
  };
};

let makeMove = (board, move, gameStatus) => {
  board
  |> List.mapi((rowIndex, row) =>
       row
       |> List.mapi((tileIndex, value) => {
            let tileId = string_of_int(rowIndex) ++ string_of_int(tileIndex);
            tileId == move
              ? switch (gameStatus, value) {
                | (Turn(_), Marked(_)) => value
                | (Turn(player), Empty) => Marked(player)
                | (_, _) => Empty
                }
              : value;
          })
     );
};