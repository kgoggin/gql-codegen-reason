open Context;

let default: js => array(File.t) =
  context => {
    let types = (context |> decode).types |> TypeDef.make;
    [|File.t(~filename="thing.re", ~content=types)|];
  };
