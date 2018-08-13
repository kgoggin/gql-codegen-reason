open Context;

open Types;

let default: js => array(File.t) =
  jsContext => {
    let context = jsContext |> decode;
    let types = Array.concat([context.types, context.inputTypes]);
    let typeDefs =
      <TypeDef types enums=context.enums />
      |> ReactDOMServerRe.renderToStaticMarkup;
    let decoders = <Decoder types /> |> ReactDOMServerRe.renderToStaticMarkup;
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
