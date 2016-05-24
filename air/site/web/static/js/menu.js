import React from "react";

export const Menu = (props) => <div id="task-menu">{props.children}</div>;

Menu.propTypes = {children: React.PropTypes.node};

export const MenuButton = (props) =>
  <button
    type="button"
    className="btn btn-primary"
    onClick={props.onClick}
    disabled={!props.isActive}
  >
    {props.children}
  </button>;

MenuButton.propTypes = {
  onClick: React.PropTypes.func.isRequired,
  isActive: React.PropTypes.bool.isRequired,
  children: React.PropTypes.node,
};

export const PaneSelectButton = (props) => {
  const classes = props.isActive ? "selection active" : "selection";

  return (
    <button onClick={props.onClick} className={classes}>
      {props.children}
    </button>
  );
};

PaneSelectButton.propTypes = {
  isActive: React.PropTypes.bool.isRequired,
  onClick: React.PropTypes.func.isRequired,
  children: React.PropTypes.node,
};

export const InfoBox = (props) => {
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

InfoBox.propTypes = {
  info: React.PropTypes.shape({
    action: React.PropTypes.func,
    message: React.PropTypes.string,
  }),
};
