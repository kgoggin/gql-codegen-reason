module Lodash = {
  [@bs.module "lodash"] external camelCase : string => string = "camelCase";
  [@bs.module "lodash"] external upperFirst : string => string = "upperFirst";
  let pascalCase = str => str |> camelCase |> upperFirst;
};

module Reason = {
  type ast;
  [@bs.module "reason"] external parseRE : string => ast = "parseRE";
  [@bs.module "reason"] external printRE : ast => string = "printRE";
};
