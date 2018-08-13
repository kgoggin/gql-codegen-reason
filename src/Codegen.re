open Context;

open Types;

let default: js => array(File.t) =
  jsContext => {
    let context = jsContext |> decode;
    let typeDefs =
      TypeDef.make(
        Array.concat([context.types, context.inputTypes]),
        context.enums,
      );
    let decoders =
      Decode.make(
        Array.concat([context.types, context.inputTypes]),
        context.enums,
      );
    let encoders =
      Encode.make(
        Array.concat([context.types, context.inputTypes]),
        context.enums,
      );
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
