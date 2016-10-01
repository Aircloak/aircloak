// @flow

import React from "react";

type Info = {
  action: () => void;
  message: string;
};

export const Menu = (props: {children: Node}) => <div id="task-menu">{props.children}</div>;

export const MenuButton = (props: {onClick: () => void, isActive: boolean, children: Node}) =>
  <button
    type="button"
    className="btn btn-primary"
    onClick={props.onClick}
    disabled={!props.isActive}
  >
    {props.children}
  </button>;

export const PaneSelectButton = (props: {isActive: boolean, onClick: () => void, children: Node}) => {
  const classes = props.isActive ? "selection active" : "selection";

  return (
    <button onClick={props.onClick} className={classes}>
      {props.children}
    </button>
  );
};

export const InfoBox = (props: {info: Info}) => {
  if (props.info == null) {
    return null;
  } else {
    return (
      <div id="infobox">
        <span className="label label-default">Notice</span>
        <span onClick={props.info.action} id="infobox-message">{props.info.message}</span>
      </div>
    );
  }
};
