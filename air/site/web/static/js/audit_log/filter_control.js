import React from "react";

export const FilterControl = (props) =>
  <div className={props.className}>
    <span>Filter audit log</span>&nbsp;
    <input
      type="text"
      value={props.value}
      onChange={props.onChange}
    />
  </div>;

FilterControl.propTypes = {
  className: React.PropTypes.string,
  value: React.PropTypes.string,
  onChange: React.PropTypes.func,
};
