import React from "react"

export class StatusLine extends React.Component {
  render() {
    return (
      <div id="task-status-bar">
        <SaveButton {...this.props} />
        <RunButton {...this.props} />
      </div>
    );
  }
}

class SaveButton extends React.Component {
  constructor(props) {
    super(props);
    this.handleSaveTaskClick = this.handleSaveTaskClick.bind(this);
  }

  handleSaveTaskClick() {
    if (!this.props.isSavedCheck()) {
      this.props.onTaskSaveClick();
    }
  }

  render() {
    var classNames = "btn btn-primary";
    if (this.props.isSavedCheck()) {
      classNames += " disabled";
    }
    return (
      <div className="task-status-bar-button-group">
        <button type="button" className={classNames} onClick={this.handleSaveTaskClick}>
          Save task
        </button>
      </div>
    );
  }
}

class RunButton extends React.Component {
  render() {
    if (this.props.runningPercent < 0) {
      var classNames = "btn btn-primary";
      if (!this.props.canRunCheck()) {
        classNames += " disabled";
      }
      return (
        <div className="task-status-bar-button-group">
          <button type="button" className={classNames} onClick={this.props.onRunTaskClick}>
            Run task
          </button>
        </div>
      );
    } else {
      return (
        <div className="task-status-bar-button-group">
          <button type="button" className="btn btn-primary disabled">
            Run task
          </button>
          <div className="progress">
            <div className="progress-bar" role="progressbar"
                aria-valuenow="{this.props.runningPercent}" aria-valuemin="0"
                aria-valuemax="100" style={{width: this.props.runningPercent + '%'}}>
              {this.props.runningPercent}%
            </div>
          </div>
        </div>
      );
    }
  }
}
