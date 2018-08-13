open Context;

open TypeWrapper;

open Types;

let fieldDecoder = (field: Field.t) =>
  switch (field.type_) {
  | ID
  | String => "Json.Decode.string"
  | Int => "Json.Decode.int"
  | Float => "Json.Decode.float"
  | Bool => "Json.Decode.bool"
  | Custom(n) =>
    let typeName = n |> Lodash.pascalCase;
    {j|json => decode($typeName, json) |> fromNode|j};
  };

module Enum = {
  let component = ReasonReact.statelessComponent("DecodeEnum");
  let make = (~enum: Enum.t, _children) => {
    ...component,
    render: _self => {
      let pascal = enum.name |> Lodash.pascalCase;
      let camel = enum.name |> Lodash.camelCase;
      let converterName = camel ++ "FromJs";
      {j|| $pascal =>
					let str = Json.Decode.string(json);
					str |> $converterName |> toNode;
			|j}
      |> ReasonReact.string;
    },
  };
};

module DecodeField = {
  let component = ReasonReact.statelessComponent("DecodeField");
  let make = (~field: Field.t, _children) => {
    ...component,
    render: _self => {
      let name = field.name |> Lodash.camelCase;
      <Fragment>
        ({j|$name: json |> |j} |> ReasonReact.string)
        <Optional>
          <FieldW>
            (name ++ ", " |> ReasonReact.string)
            <Nullable wrap=(! field.isRequired)>
              (field |> fieldDecoder |> ReasonReact.string)
            </Nullable>
          </FieldW>
        </Optional>
      </Fragment>;
    },
  };
};

let component = ReasonReact.statelessComponent("Decoder");

let make = (~types, _children) => {
  ...component,
  render: _self =>
    <Fragment>
      (
        {j|let rec decode: (gqlType, Js.Json.t) => node =
					(t, json) =>
						switch (t) {
				|j}
        |> ReasonReact.string
      )
      (
        types
        |> Array.map((t: Type.t) => {
             let name = t.name |> stringOfFieldType |> Lodash.pascalCase;
             <Fragment>
               ({j|| $name => { |j} |> ReasonReact.string)
               (
                 t.fields
                 |> Array.map(field => <DecodeField field />)
                 |> ReasonReact.array
               )
               ({j|} |> toNode|j} |> ReasonReact.string)
             </Fragment>;
           })
        |> ReasonReact.array
      )
      (
        {j|};
				let decodeType: (gqlType, Js.Json.t) => 'a =
				  (t, j) => decode(t, j) |> fromNode;|j}
        |> ReasonReact.string
      )
    </Fragment>,
};
