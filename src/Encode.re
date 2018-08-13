open Context;

open Types;

let nodeRecordVariableName = "r";

let renderField = (prev, field: Field.t) => {
  let name = field.name;
  if (field.type_ === ID && name === "id" && field.isRequired) {
    {j|
		$prev
		("id", Json.Encode.string($nodeRecordVariableName.$name)),
		|j};
  } else {
    let wrapper =
      switch (field.type_, ! field.isRequired, field.isArray) {
      | (Custom(_n), _, true) => "optionalNodeList"
      | (Custom(_n), false, false) => "optionalNode"
      | (Custom(_n), true, false) => "optionalNullableNode"
      | (_, true, _) => "optionalNullable"
      | _ => "nullable"
      };
    let encoder =
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
    {j|
	$prev
	("$name", $wrapper($encoder, $nodeRecordVariableName.$name)),
	|j};
  };
};

let renderTypeWrapper = (name, fields) => {
  let pascalName = name |> Lodash.pascalCase;
  {j|
	| $pascalName =>
	let $nodeRecordVariableName: $name = record |> fromNode;
	Json.Encode.(
		[
			$fields
		]
		|> addTypeName("$pascalName")
		|> object_
	);
|j};
};

let renderEncodeFn = encoders => {j|
	let rec encodeNode = (gqlType, record: node) =>
	  switch (gqlType) {
			$encoders
		};
|j};

let make = (types, enums) => {
  let typeStr =
    types
    |> Array.fold_left(
         (prev, t: Type.t) =>
           switch (t.name) {
           | Custom(name) =>
             prev
             ++ (
               t.fields
               |> Array.fold_left(renderField, "")
               |> renderTypeWrapper(name)
             )
           | _ => prev
           },
         "",
       );
  let enumStr =
    enums
    |> Array.fold_left(
         (prev, enum: Enum.t) => {
           let pascal = enum.name |> Lodash.pascalCase;
           let camel = enum.name |> Lodash.camelCase;
           let converterName = camel ++ "ToJs";
           {j|$prev
					| $pascal => record |> fromNode |> $converterName |> Json.Encode.string
			|j};
         },
         typeStr,
       );
  renderEncodeFn(enumStr);
};
