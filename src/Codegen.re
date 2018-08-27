open Context;

[@bs.val] external decodeURI: string => string = "";

let default: js => array(File.t) =
  jsContext => {
    let context = jsContext |> decode;
    let types = Array.concat([context.types, context.inputTypes]);
    let typeDefs =
      <TypeDef types enums={context.enums} />
      |> ReactDOMServerRe.renderToStaticMarkup;
    let decoders =
      <Decoder types /> |> ReactDOMServerRe.renderToStaticMarkup |> decodeURI;
    let encoders =
      <Encoder types /> |> ReactDOMServerRe.renderToStaticMarkup |> decodeURI;
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
		|j},
        /* |> Reason.parseRE
           |> Reason.printRE, */
      ),
    |];
  };
