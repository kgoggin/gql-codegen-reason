open Types;

open Context;

open TypeWrapper;

module Field = {
  let make = (~field: Field.t, _children) => {
    let name = field.name |> Lodash.camelCase;
    {j|$name: |j}
    ++ OptionW.make(
         Nullable.make(
           ~wrap=!field.isRequired,
           field.type_ |> stringOfFieldType,
         ),
       );
  };
};

module TypeDef = {
  let make = (~t: Type.t, _children) => {
    let name = t.name |> stringOfFieldType |> Lodash.camelCase;
    let fields = t.fields |> Array.map(field => Field.make(~field));
    {j|$name = {$fields}|j};
  };
};

let make = (~types as typesArr, _children) => {
  let types =
    typesArr
    |> Array.mapi((idx, t) =>
         TypeDef.make(~t, ())
         ++ (idx + 1 === Array.length(typesArr) ? "" : " and ")
       );
  {j|type $types|j};
};
