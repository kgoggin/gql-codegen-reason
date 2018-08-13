let component = ReasonReact.statelessComponent("TypeDef");

let make = (~enums, ~types, _children) => {
  ...component,
  render: _self =>
    <Fragment> <Enums enums /> <RecursiveTypes types /> </Fragment>,
};
