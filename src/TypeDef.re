open Context;

let wrapWithOption = s => {j|option($s)|j};

let wrapWithList = (shouldWrap, s) => shouldWrap ? {j|list($s)|j} : s;

let fieldDef = (f: Field.t) => {
  let fieldName = f.name;
  let fieldType =
    f.type_
    |> stringOfNullableFieldType
    |> wrapWithList(f.isArray)
    |> wrapWithOption;
  {j|$fieldName: $fieldType|j};
};

let typeDef = (name, fields) => {
  let fieldStr = fields |> Array.map(fieldDef);
  {j|$name = {
		$fieldStr
	}|j};
};

let make = types => {
  let typesStr =
    types
    |> Array.map((t: Type.t) =>
         switch (t.name) {
         | Custom(n) => typeDef(n, t.fields)
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
  {j|$final;|j};
};
