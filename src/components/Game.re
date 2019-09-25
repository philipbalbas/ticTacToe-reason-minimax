open Shared;

type score = int;

type tally = {
  cross: score,
  circle: score,
  tie: score,
};

type state = {
  board,
  gameStatus,
  tally,
};

type action =
  | Mark(string)
  | Start
  | Rematch;

let initialState = {
  board: [
    [Empty, Empty, Empty],
    [Empty, Empty, Empty],
    [Empty, Empty, Empty],
  ],
  gameStatus: None,
  tally: {
    cross: 0,
    circle: 0,
    tie: 0,
  },
};

[@react.component]
let make = () => {
  let ({board, gameStatus, tally}, dispatch) =
    React.useReducer(
      (state, action) =>
        switch (action) {
        | Start => {...state, gameStatus: Turn(Human)}
        | Rematch => {
            ...state,
            board: initialState.board,
            gameStatus: Turn(Human),
          }
        | Mark(tileId) =>
          switch (state.gameStatus) {
          | Won(_) => state
          | _ =>
            let updatedBoard =
              makeMove(state.board, tileId, state.gameStatus);
            let flattened = updatedBoard |> List.flatten;

            let updateGameStatus =
                (~prevBoard, ~updatedBoard, ~prevGameStatus, ~winTable) => {
              prevBoard == updatedBoard
                ? prevGameStatus
                : {
                  checkGameStatus(winTable, flattened, state.gameStatus);
                };
            };

            let updatedGameStatus =
              updateGameStatus(
                ~prevBoard=state.board,
                ~updatedBoard,
                ~prevGameStatus=state.gameStatus,
                ~winTable,
              );

            let updatedTally =
              switch (updatedGameStatus) {
              | Won(Human) => {...state.tally, cross: state.tally.cross + 1}
              | Won(Computer) => {
                  ...state.tally,
                  circle: state.tally.circle + 1,
                }
              | Tie => {...state.tally, tie: state.tally.tie + 1}
              | _ => state.tally
              };

            {
              tally: updatedTally,
              gameStatus: updatedGameStatus,
              board: updatedBoard,
            };
          }
        },
      initialState,
    );

  React.useEffect1(
    () => {
      if (gameStatus == Turn(Computer)) {
        let move = board->AI.getBestMove;
        dispatch(Mark(move));
      };
      None;
    },
    [|gameStatus|],
  );

  let currentPlayer =
    switch (gameStatus) {
    | Turn(Human) => "You"
    | Turn(Computer) => "Computer"
    | _ => ""
    };

  let menu =
    switch (gameStatus) {
    | None =>
      <button onClick={_ => dispatch(Start)}>
        "Start Game"->React.string
      </button>
    | Turn(Human) => <div> {j|Playing: $currentPlayer|j}->React.string </div>
    | Turn(Computer) =>
      <div> {j|Playing: $currentPlayer|j}->React.string </div>
    | Won(user) =>
      let wonText =
        switch (user) {
        | Human => "You beat the computer!"
        | Computer => "All hail the machine!"
        };
      <>
        <div> wonText->React.string </div>
        <button onClick={_ => dispatch(Rematch)}>
          "Get a rematch"->React.string
        </button>
      </>;
    | Tie =>
      <>
        <div> "It's a draw"->React.string </div>
        <button onClick={_ => dispatch(Rematch)}>
          "Get a rematch"->React.string
        </button>
      </>
    };

  let {cross, circle, tie} = tally;

  <div className="container">
    <div>
      <Board board gameStatus setMark={id => dispatch(Mark(id))} />
      <div> menu </div>
      <div className="scoreboard">
        <div> {j|Player: $cross|j}->React.string </div>
        <div> {j|Tie: $tie|j}->React.string </div>
        <div> {j|Computer: $circle|j}->React.string </div>
      </div>
    </div>
  </div>;
};