import React from "react"

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

export class PaneView extends React.Component {
  render() {
    if (this.props.isActive()) {
      return (
        <div id="side-pane">
          <div id="side-pane-hide">
            <button onClick={this.props.onHideClick} className="btn">
              Hide sidepane >
            </button>
          </div>
          <div id="side-pane-content">
            {this.props.children}
          </div>
        </div>
      );
    } else {
      return null;
    }
  }
}
