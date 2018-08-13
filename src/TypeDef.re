open Context;

open Types;

let wrapWithOption = (shouldWrap, s) => shouldWrap ? {j|option($s)|j} : s;

let wrapWithList = (shouldWrap, s) => shouldWrap ? {j|list($s)|j} : s;

let fieldDef = (isInputType, f: Field.t) => {
  let fieldName = f.name;
  let unwrappedFieldType =
    f.type_ |> stringOfNullableFieldType |> wrapWithList(f.isArray);
  let shouldWrapWithOption = ft =>
    ! isInputType
    && (
      switch (ft) {
      | ID => f.name === "id"
      | _ => false
      }
    );
  let fieldType =
    switch (f.type_) {
    | Nullable(fieldType) =>
      unwrappedFieldType |> wrapWithOption(fieldType |> shouldWrapWithOption)
    | NonNullable(fieldType) =>
      unwrappedFieldType |> wrapWithOption(fieldType |> shouldWrapWithOption)
    };
  {j|$fieldName: $fieldType|j};
};

let typeDef = (name, fields, isInputType) => {
  let fieldStr = fields |> Array.map(fieldDef(isInputType));
  {j|$name = {
		$fieldStr
	}|j};
};

let renderEnum = (prev, enum: Enum.t) => {
  let values =
    enum.values
    |> Array.fold_left(
         (prev, value: Enum.value) => {
           let upper = value.name |> Lodash.toUpper;
           let pascal = value.name |> Lodash.pascalCase;
           {j|$prev | [@bs.as "$upper"] `$pascal|j};
         },
         "",
       );
  let name = enum.name |> Lodash.camelCase;
  {j|$prev
		[@bs.deriving jsConverter]
		type $name = [$values];
	|j};
};

let make = (types, enums) => {
  let enumStr = enums |> Array.fold_left(renderEnum, "");
  let typesStr =
    types
    |> Array.map((t: Type.t) =>
         switch (t.name) {
         | Custom(n) => typeDef(n, t.fields, t.isInputType)
         | _ => ""
         }
       )
    |> Array.fold_left(
         (str, td) =>
           switch (td) {
           | "" => str
           | _ => str ++ td ++ " and "
           },
         "type ",
       );
  /* take out the last "and" */
  let final = String.sub(typesStr, 0, String.length(typesStr) - 5);
  {j|$enumStr
		$final;|j};
};
