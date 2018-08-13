module Base = {
  let component = ReasonReact.statelessComponent("OptionWrapper");
  let make = (~wrapper, ~wrap=true, children) => {
    ...component,
    render: _self => {
      let chldrn = <Fragment> ...children </Fragment>;
      wrap ?
        <Fragment>
          ({j|$wrapper("|j} |> ReasonReact.string)
          chldrn
          ({j|")"|j} |> ReasonReact.string)
        </Fragment> :
        chldrn;
    },
  };
};

module Null = {
  let component = ReasonReact.statelessComponent("NullWrapper");
  let make = (~wrap=true, children) => {
    ...component,
    render: _self => <Base wrapper="Js.null" wrap> ...children </Base>,
  };
};

module Nullable = {
  let component = ReasonReact.statelessComponent("NullableWrapper");
  let make = (~wrap=true, children) => {
    ...component,
    render: _self => <Base wrapper="nullable" wrap> ...children </Base>,
  };
};

module OptionW = {
  let component = ReasonReact.statelessComponent("OptionWrapper");
  let make = (~wrap=true, children) => {
    ...component,
    render: _self => <Base wrapper="option" wrap> ...children </Base>,
  };
};

module Optional = {
  let component = ReasonReact.statelessComponent("OptionalWrapper");
  let make = (~wrap=true, children) => {
    ...component,
    render: _self => <Base wrapper="optional" wrap> ...children </Base>,
  };
};

module FieldW = {
  let component = ReasonReact.statelessComponent("FieldWrapper");
  let make = (~wrap=true, children) => {
    ...component,
    render: _self => <Base wrapper="Field" wrap> ...children </Base>,
  };
};
