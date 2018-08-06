open Context;

open Types;

let nodeRecordVariableName = "r";

let renderField = (prev, field: Field.t) => {
  let name = field.name;
  let (isNullable, fieldType) =
    switch (field.type_) {
    | Nullable(kind) => (true, kind)
    | NonNullable(kind) => (false, kind)
    };
  let wrapper =
    switch (fieldType, isNullable, field.isArray) {
    | (Custom(_n), _, true) => "optionalNodeList"
    | (Custom(_n), false, false) => "nullable"
    | (Custom(_n), true, false) => "optionalNullable"
    | _ => "nullable"
    };
  let encoder =
    switch (fieldType) {
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

let renderTypeWrapper = (name, fields) => {
  let pascalName = name |> Lodash.pascalCase;
  {j|
	| $pascalName =>
	let $nodeRecordVariableName: $name = record |> fromNode;
	Json.Encode(
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

let make = types =>
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
     )
  |> renderEncodeFn;
