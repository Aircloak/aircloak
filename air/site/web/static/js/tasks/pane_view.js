import React from "react";

export class PaneView extends React.Component {
  render() {
    if (this.props.isActive()) {
      return (
        <div id="side-pane">
          <div id="side-pane-hide">
            <button onClick={this.props.onHideClick} className="btn">
              Hide sidepane &gt;
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

PaneView.propTypes = {
  isActive: React.PropTypes.func.isRequired,
  onHideClick: React.PropTypes.func.isRequired,
  children: React.PropTypes.node,
};
