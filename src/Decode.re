open Context;

open Types;

let renderField = (prev, field: Field.t) => {
  let name = field.name;
  let (wrapper, fieldType) =
    switch (field.type_) {
    | Nullable(kind) => ("optionalNullableField", kind)
    | NonNullable(kind) => ("optionalField", kind)
    };
  let baseDecoder =
    switch (fieldType) {
    | String => "Json.Decode.string"
    | Int => "Json.Decode.int"
    | Float => "Json.Decode.float"
    | Bool => "Json.Decode.bool"
    | Custom(n) =>
      let typeName = n |> Lodash.pascalCase;
      {j|json => decode($typeName, json) |> fromNode|j};
    };
  let finalDecoder = field.isArray ? {j|list($baseDecoder)|j} : baseDecoder;
  {j|
	$prev
	$name: json |> $wrapper("$name", $finalDecoder),
	|j};
};

let renderTypeWrapper = (typeName, fields) => {j|
	| $typeName =>
		Json.Decode.{
			$fields
		} |> toNode
|j};

let renderGQLVariants = names => {j|type gqlType = $names;|j};

let renderEnum = (prev, enum: Enum.t) => {
  let pascal = enum.name |> Lodash.pascalCase;
  let camel = enum.name |> Lodash.camelCase;
  let converterName = camel ++ "FromJs";
  {j|$prev
	| $pascal =>
		let str = Json.Decode.string(json);
		str |> $converterName |> toNode;
	|j};
};

let renderDecodeFn = decoders => {j|
		let rec decode: (gqlType, Js.Json.t) => node =
		(t, json) =>
			switch (t) {$decoders
			};

		let decodeType: (gqlType, Js.Json.t) => 'a =
		  (t, j) => decode(t, j) |> fromNode;
	|j};

let make = (types, enums) => {
  let (typeNames, decoders) =
    types
    |> Array.fold_left(
         ((prevV, prevD), t: Type.t) =>
           switch (t.name) {
           | Custom(name) =>
             let pascal = name |> Lodash.pascalCase;
             (
               {j|$prevV | $pascal |j},
               prevD
               ++ (
                 t.fields
                 |> Array.fold_left(renderField, "")
                 |> renderTypeWrapper(pascal)
               ),
             );
           | _ => (prevV, prevD)
           },
         ("", ""),
       );
  let typeNamesIncludingEnums =
    enums
    |> Array.fold_left(
         (prev, enum: Enum.t) => {
           let pascal = enum.name |> Lodash.pascalCase;
           {j|$prev | $pascal |j};
         },
         typeNames,
       );
  let decodersIncludingEnums = enums |> Array.fold_left(renderEnum, decoders);
  renderGQLVariants(typeNamesIncludingEnums)
  ++ " "
  ++ renderDecodeFn(decodersIncludingEnums);
};
