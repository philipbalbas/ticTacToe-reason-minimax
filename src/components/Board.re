open Shared;

module Tile = {
  [@react.component]
  let make = (~tile, ~id, ~gameStatus, ~setMark) => {
    let symbol =
      switch (tile) {
      | Empty => React.null
      | Marked(Human) => <span className="cross"> "X"->React.string </span>
      | Marked(Computer) =>
        <span className="circle"> "O"->React.string </span>
      };

    <div className="item">
      <button onClick={_ => setMark(id)}> symbol </button>
    </div>;
  };
};

module Row = {
  [@react.component]
  let make = (~row, ~rowIndex, ~gameStatus, ~setMark) => {
    row
    |> List.mapi((index, tile) => {
         let id = string_of_int(rowIndex) ++ string_of_int(index);
         <Tile tile key={index->string_of_int} id gameStatus setMark />;
       })
    |> Array.of_list
    |> React.array;
  };
};

[@react.component]
let make = (~board, ~gameStatus, ~setMark) => {
  let boardRow =
    board
    |> List.mapi((index, row) =>
         <Row
           key={index->string_of_int}
           row
           rowIndex=index
           gameStatus
           setMark
         />
       )
    |> Array.of_list
    |> React.array;
  <div className="board"> boardRow </div>;
};