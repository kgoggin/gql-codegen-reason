open Types;

open Context;

open TypeWrapper;

module Field = {
  let component = ReasonReact.statelessComponent("Field");
  let make = (~field: Field.t, _children) => {
    ...component,
    render: _self => {
      let name = field.name |> Lodash.camelCase;
      <Fragment>
        ({j|$name: |j} |> ReasonReact.string)
        <OptionW>
          <Nullable wrap=(! field.isRequired)>
            (field.type_ |> stringOfFieldType |> ReasonReact.string)
          </Nullable>
        </OptionW>
      </Fragment>;
    },
  };
};

module TypeDef = {
  let component = ReasonReact.statelessComponent("TypeDef");
  let make = (~t: Type.t, _children) => {
    ...component,
    render: _self => {
      let name = t.name |> stringOfFieldType |> Lodash.camelCase;
      <Fragment>
        ({j|$name = { |j} |> ReasonReact.string)
        (t.fields |> Array.map(field => <Field field />) |> ReasonReact.array)
        ("}" |> ReasonReact.string)
      </Fragment>;
    },
  };
};

let component = ReasonReact.statelessComponent("RecursiveTypes");

let make = (~types, _children) => {
  ...component,
  render: _self =>
    <Fragment>
      ({j|type |j} |> ReasonReact.string)
      (
        types
        |> Array.mapi((idx, t) =>
             <Fragment>
               <TypeDef t />
               (
                 idx + 1 === Array.length(types) ?
                   ReasonReact.null : " and " |> ReasonReact.string
               )
             </Fragment>
           )
        |> ReasonReact.array
      )
    </Fragment>,
};
