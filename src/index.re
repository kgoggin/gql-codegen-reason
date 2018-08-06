open Context;

open Types;

let default: js => array(File.t) =
  context => {
    let types = (context |> decode).types;
    let typeDefs = TypeDef.make(types);
    let decoders = Decode.make(types);
    let encoders = Encode.make(types);
    [|
      File.t(
        ~filename="thing.re",
        ~content={j|
			$typeDefs

			$decoders

			$encoders
		|j},
      ),
    |];
  };
