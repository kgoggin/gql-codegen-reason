open Context;

open Types;

let default: js => array(File.t) =
  context => {
    let types = (context |> decode).types;
    let typeDefs = TypeDef.make(types);
    let decoders = Decode.make(types);
    let encoders = Encode.make(types);
    let static = StaticHelpers.make();
    [|
      File.t(
        ~filename="GQLTypes.re",
        ~content=
          {j|
			$static

			$typeDefs

			$decoders

			$encoders
		|j}
          |> Reason.parseRE
          |> Reason.printRE,
      ),
    |];
  };
