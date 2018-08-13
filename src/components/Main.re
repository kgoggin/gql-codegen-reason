let component = ReasonReact.statelessComponent("Main");
let make = _children => {
  ...component,
  render: _self => <Option> ("author" |> ReasonReact.string) </Option>,
};

let default = ReasonReact.wrapReasonForJs(~component, _jsProps => make());
