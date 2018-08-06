let make = () => {j|
type node;

external toNode : 'a => node = "%identity";

external fromNode : node => 'a = "%identity";

exception IdNotFetched(string);

let optionalField = (fieldName, decoder) =>
  Json.Decode.(optional(field(fieldName, decoder)));

let optionalNullableField = (fieldName, decoder) =>
  Json.Decode.(optional(field(fieldName, nullable(decoder))));

let idField = (typeName, json) =>
  switch (Json.Decode.(field("id", string, json))) {
  | id => id
  | exception (Json.Decode.DecodeError(_)) =>
    raise(IdNotFetched("id not fetched for: " ++ typeName))
  };

	let optionalNodeList =
	    (encoder: Json.Encode.encoder(node), data: option(list('a))) =>
	  switch (data) {
	  | None => Json.Encode.null
	  | Some(d) => Json.Encode.list(encoder, d |> List.map(toNode))
	  };

	let optionalNode = (encoder: Json.Encode.encoder(node), data: option('a)) =>
	  switch (data) {
	  | None => Json.Encode.null
	  | Some(d) => encoder(d |> toNode)
	  };

	let optionalNullableNode =
	    (encoder: Json.Encode.encoder(node), data: option(Js.Null.t('a))) => {
	  let opt =
	    switch (data) {
	    | None => None
	    | Some(nullableData) => nullableData |> Js.Null.toOption
	    };
	  optionalNode(encoder, opt);
	};

	let optionalNullable =
	    (encoder: Json.Encode.encoder('a), data: option(Js.Null.t('a))) => {
	  let opt =
	    switch (data) {
	    | None => None
	    | Some(nullableData) => nullableData |> Js.Null.toOption
	    };
	  Json.Encode.nullable(encoder, opt);
	};

	let addTypeName = (name, list: list((string, Js.Json.t))) =>
	  list @ [("__typename", Json.Encode.string(name))];
|j};
