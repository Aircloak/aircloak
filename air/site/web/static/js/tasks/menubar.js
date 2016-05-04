import React from "react"

export class MenuBar extends React.Component {
  render() {
    return (
      <div id="task-status-bar">
        {this.props.children}
      </div>
    );
  }
}

export class MenuButton extends React.Component {
  render() {
    var classNames = "btn btn-primary";
    if (!this.props.isActive()) {
      classNames += " disabled";
    }
    return (
      <button type="button" className={classNames} onClick={this.props.onClick}>
        {this.props.children}
      </button>
    );
  }

}

export class TaskProgress extends React.Component {
  render() {
    if (this.props.runningPercent < 0) {
      return null;
    } else {
      return (
        <div className="progress">
          <div className="progress-bar" role="progressbar"
              aria-valuenow="{this.props.runningPercent}" aria-valuemin="0"
              aria-valuemax="100" style={{width: this.props.runningPercent + '%'}}>
            {this.props.runningPercent}%
          </div>
        </div>
      );
    }
  }
}

export class PaneSelectButton extends React.Component {
  render() {
    let classes = "selection";
    if (this.props.isActive()) {
      classes = classes + " active";
    }
    return (
      <button onClick={this.props.onClick} className={classes}>
        {this.props.children}
      </button>
    )
  }
}
