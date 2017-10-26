// @flow

import React from "react";

export const Menu = (props: {children: Node}) => <div id="task-menu">{props.children}</div>;

export const MenuButton = (props: {onClick: () => void, isActive: boolean, children?: Node}) =>
  <button
    type="button"
    className="btn btn-primary"
    onClick={props.onClick}
    disabled={!props.isActive}
  >
    {props.children}
  </button>;
