open Context;

open Types;

module Enum = {
  let component = ReasonReact.statelessComponent("Enum");
  let make = (~enum: Enum.t, _children) => {
    ...component,
    render: _self => {
      let name = enum.name |> Lodash.camelCase;
      <Fragment>
        (
          {j|
								[@bs.deriving jsConverter]
								type $name = |j}
          |> ReasonReact.string
        )
        (
          enum.values
          |> Array.map((value: Enum.value) => {
               let upper = value.name |> Lodash.toUpper;
               let pascal = value.name |> Lodash.pascalCase;
               {j|| [@bs.as "$upper"] `$pascal|j} |> ReasonReact.string;
             })
          |> ReasonReact.array
        )
        (";" |> ReasonReact.string)
      </Fragment>;
    },
  };
};



let component = ReasonReact.statelessComponent("Enums");

let make = (~enums, _children) => {
  ...component,
  render: _self =>
    enums
    |> Array.map(enum =>
         <Fragment> <Enum enum /> (" " |> ReasonReact.string) </Fragment>
       )
    |> ReasonReact.array,
};
