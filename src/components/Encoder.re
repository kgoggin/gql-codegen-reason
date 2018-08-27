open Context;

open Types;

open TypeWrapper;

let nodeRecordVariableName = "r";

let fieldIsOptional = _field => true;

let fieldEncoder = (field: Field.t) =>
  switch (field.type_) {
  | ID
  | String => "Json.Encode.string"
  | Int => "Json.Encode.int"
  | Float => "Json.Encode.float"
  | Bool => "Json.Encode.bool"
  | Custom(n) =>
    let typeName = n |> Lodash.pascalCase;
    {j|encodeNode($typeName)|j};
  };

/**
 * nullable/nullableNode
 * - wrapped in option, isRequired === true
 *
 * optionalNullable/optionalNullableNode
 * - wrapped in option, isRequired === false
 *
 * optionalNodeList/optionalList
 * - wrapped in option, isArray
 *
 * optionalList = nullable(list())
 * optionalNullabe  = nullable(nullable())
 */
module EncodeScalarField = {
  let make = (~field: Field.t, _children) => {
    let name = field.name |> Lodash.camelCase;
    let encoder = field |> fieldEncoder;
    let recordField = {j|$nodeRecordVariableName.$name|j};
    let isWrappedWithOption = field |> fieldIsOptional;
    let f =
      !isWrappedWithOption && field.isRequired && !field.isArray ?
        TypeWrapper.Base.make(~wrapper=encoder, recordField) :
        Nullable.make(
          ~wrap=isWrappedWithOption,
          Nullable.make(
            ~wrap=!field.isRequired,
            Base.make(
              ~wrapper="list",
              ~wrap=field.isArray,
              {j|$encoder, $recordField|j},
            ),
          ),
        );
    {j|($name, $f),|j};
  };
};

module EncodeNodeField = {
  let make = (~field: Field.t, _children) => {
    let name = field.name |> Lodash.camelCase;
    let encoder = field |> fieldEncoder;
    let recordField = {j|$nodeRecordVariableName.$name|j};
    let isWrappedWithOption = field |> fieldIsOptional;
    let f =
      !isWrappedWithOption && field.isRequired && !field.isArray ?
        TypeWrapper.Base.make(~wrapper=encoder, recordField) :
        {
          let wrapper =
            switch (isWrappedWithOption, field.isRequired, field.isArray) {
            | (true, true, false) => "optionalNode"
            | (true, false, false) => "optionalNullableNode"
            | (true, true, true) => "optionalNodeList"
            | _ =>
              Js.log(field.name);
              raise(Not_found);
            };
          TypeWrapper.Base.make(~wrapper, {j|$encoder, $recordField|j});
        };
    {j|($name, $f),|j};
  };
};

let component = ReasonReact.statelessComponent("Encoder");

let make = (~types, _children) => {
  ...component,
  render: _self =>
    <Fragment>
      {
        {j|let rec encodeNode = (gqlType, record: node) =>
						switch (gqlType) { |j}
        |> ReasonReact.string
      }
      {
        types
        |> Array.map((t: Type.t) => {
             let pascalName = t.name |> stringOfFieldType |> Lodash.pascalCase;
             let name = t.name |> stringOfFieldType |> Lodash.camelCase;
             <Fragment>
               {
                 {j|| $pascalName =>
											let $nodeRecordVariableName: $name = record |> fromNode;
											[
								|j}
                 |> ReasonReact.string
               }
               {
                 t.fields
                 |> Array.map((field: Field.t) =>
                      switch (field.type_) {
                      | Custom(_) => <EncodeNodeField field />
                      | _ => <EncodeScalarField field />
                      }
                    )
                 |> ReasonReact.array
               }
               {{j|] |> Json.Encode.object_|j} |> ReasonReact.string}
             </Fragment>;
           })
        |> ReasonReact.array
      }
      {{j|};|j} |> ReasonReact.string}
    </Fragment>,
};
