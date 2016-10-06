// @flow

import React from "react";

export const FilterControl = (props: {className: string, value: string, onChange: () => void}) =>
  <div className={props.className}>
    <span>Filter audit log</span>&nbsp;
    <input
      type="text"
      value={props.value}
      onChange={props.onChange}
    />
  </div>;
