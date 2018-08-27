module Base = {
  let make = (~wrapper, ~wrap=true, children) =>
    wrap ? {j|$wrapper($children)"|j} : children;
};

module Null = {
  let make = (~wrap=true, children) =>
    Base.make(~wrapper="Js.null", ~wrap, children);
};

module Nullable = {
  let make = (~wrap=true, children) =>
    Base.make(~wrapper="nullable", ~wrap, children);
};

module OptionW = {
  let make = (~wrap=true, children) =>
    Base.make(~wrapper="option", ~wrap, children);
};

module Optional = {
  let make = (~wrap=true, children) =>
    Base.make(~wrapper="optional", ~wrap, children);
};

module FieldW = {
  let make = (~wrap=true, children) =>
    Base.make(~wrapper="Field", ~wrap, children);
};
