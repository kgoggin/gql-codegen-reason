open Types;

type fieldType =
  | Int
  | String
  | Bool
  | Float
  | Custom(string);

type nullableFieldType =
  | Nullable(fieldType)
  | NonNullable(fieldType);

let stringToFieldType = t =>
  switch (t) {
  | "Int" => Int
  | "String" => String
  | "Boolean" => Bool
  | n => Custom(n |> Lodash.camelCase)
  };

let stringOfFieldType =
  fun
  | Int => "int"
  | String => "string"
  | Bool => "bool"
  | Float => "float"
  | Custom(t) => t;

let stringOfNullableFieldType =
  fun
  | Nullable(t) => {
      let s = t |> stringOfFieldType;
      {j|Js.null($s)|j};
    }
  | NonNullable(t) => t |> stringOfFieldType;

module Field = {
  [@bs.deriving abstract]
  type js = {
    name: string,
    description: string,
    [@bs.as "type"]
    type_: string,
    isArray: bool,
    isRequired: bool,
    isNullableArray: bool,
  };
  type t = {
    name: string,
    description: string,
    isRequired: bool,
    isArray: bool,
    type_: nullableFieldType,
  };
  let decode: js => t =
    t => {
      name: t |. name,
      description: t |. description,
      isRequired: t |. isRequired,
      isArray: t |. isArray,
      type_:
        t |. isRequired ?
          NonNullable(stringToFieldType(t |. type_)) :
          Nullable(stringToFieldType(t |. type_)),
    };
};

module Type = {
  [@bs.deriving abstract]
  type js = {
    fields: array(Field.js),
    description: string,
    name: string,
    isInputType: bool,
    hasFields: bool,
    hasInterfaces: bool,
  };
  type t = {
    description: string,
    name: fieldType,
    isInputType: bool,
    fields: array(Field.t),
  };
  let decode: js => t =
    t => {
      description: t |. description,
      isInputType: t |. isInputType,
      name: stringToFieldType(t |. name),
      fields: t |. fields |> Array.map(Field.decode),
    };
};

[@bs.deriving abstract]
type js = {types: array(Type.js)};

type t = {types: array(Type.t)};

let decode: js => t = t => {types: t |. types |> Array.map(Type.decode)};

module File = {
  [@bs.deriving abstract]
  type t = {
    filename: string,
    content: string,
  };
};
