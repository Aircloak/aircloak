// @flow

import React from "react";

export const Menu = (props: {children: ?Iterable<React$Node>}) => {
  const {children} = props;
  return <div id="task-menu">{children}</div>;
}

export const MenuButton = (props: {onClick: () => void, isActive: boolean, children?: Iterable<React$Node>}) => {
  const {children, isActive, onClick} = props;
  return (
    <button
      type="button"
      className="btn btn-primary"
      onClick={onClick}
      disabled={!isActive}
    >
      {children}
    </button>
  );
}
