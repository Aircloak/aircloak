import React from "react";

export class SidePane extends React.Component {
  render() {
    if (this.props.sidePaneHidden()) {
      return null;
    } else {
      return (
        <div id="task-editor-side-panel">
          {this.props.children}
        </div>
      );
    }
  }
}

SidePane.propTypes = {
  sidePaneHidden: React.PropTypes.func.isRequired,
  children: React.PropTypes.node,
};
