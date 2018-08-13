let component = ReasonReact.statelessComponent("Option");
let make = (children: array(ReasonReact.reactElement)) => {
  ...component,
  render: _self =>
    <Fragment>
      ("option(\"" |> ReasonReact.string)
      <Fragment> ...children </Fragment>
      ("\")" |> ReasonReact.string)
    </Fragment>,
};
