import React from "react"
import ReactDOM from "react-dom"
import { CodeEditor } from "./code_editor"

class TaskEditor extends React.Component {
  constructor(props) {
    super();

    this.state = {
      // active state used in the query editor
      // and task name field
      query: props.query,
      name: props.name,

      // We keep some stats on whether or not
      // the task is running. This is used
      // throughout the interface to toggle
      // buttons and show progress bars
      runningPercent: -1,
    }
    // we keep a backup copy of the last saved
    // state of the task, in order to be able to
    // verify whether the task needs saving or not
    this.state.savedState = this.queryData();

    // Bind the handlers, so we can pass them, and have `this`
    // bound to the right context
    this.checkForUnsavedChanges = this.checkForUnsavedChanges.bind(this);
    this.handleNameChange = this.handleNameChange.bind(this);
    this.handleCodeChange = this.handleCodeChange.bind(this);
    this.handleRunTask = this.handleRunTask.bind(this);
    this.isSaved = this.isSaved.bind(this);
    this.saveTask = this.saveTask.bind(this);
    this.conditionallySave = this.conditionallySave.bind(this);

    // To prevent the user loosing changes, we ask whether
    // the page should be closed, if changes have been
    // made since the last time the task was saved.
    window.onbeforeunload = this.checkForUnsavedChanges;
  }
  checkForUnsavedChanges() {
    if (!this.isSaved()) {
      return "Your task has unsaved changes!";
    }
  }
  handleNameChange(name) {
    this.setState({name: name});
  }
  handleCodeChange(query) {
    this.setState({query: query});
  }
  updateSavedState() {
    this.setState({savedState: this.queryData()});
  };
  queryData() {
    return {
      task: {
        query: this.state.query,
        name: this.state.name
      }
    };
  }
  conditionallySave() {
    if (!this.isSaved()) {
      this.saveTask()
    }
  }
  isSaved() {
    // Hack to get object comparison in order to see whether or not the object is changed
    return (JSON.stringify(this.state.savedState) == JSON.stringify(this.queryData()));
  }
  handleRunTask() {
    this.setState({runningPercent: 0});
    $.ajax(`/tasks/${this.props.id}/run`, {
          context: this,
          method: "POST",
          headers: {
            "X-CSRF-TOKEN": this.props.CSRFToken
          },
          data: this.queryData(),
          success: (responseData, textStatus) => {
            console.log("Task run scheduled...");
          }
        });
  }
  saveTask() {
    $.ajax(`/tasks/${this.props.id}`, {
          context: this,
          method: "PUT",
          headers: {
            "X-CSRF-TOKEN": this.props.CSRFToken
          },
          data: this.queryData(),
          success: this.updateSavedState,
          error: (jqXHR, textStatus) => {
            console.log(`Error: ${textStatus}`);
          }
      });
  }
  render() {
    return (
      <div id="task-editor-container">
        <StatusLine
            runningPercent={this.state.runningPercent}
            name={this.state.name}
            isSavedCheck={this.isSaved}
            onNameChange={this.handleNameChange}
            onTaskSaveClick={this.saveTask}
            onRunTaskClick={this.handleRunTask} />
        <CodeEditor
            query={this.state.query}
            onChange={this.handleCodeChange}
            onSave={this.conditionallySave} />
        <SidePane />
      </div>
    );
  }
}

class StatusLine extends React.Component {
  constructor(props) {
    super(props);
    this.handleNameChange = this.handleNameChange.bind(this);
  }
  handleNameChange(event) {
    name = event.target.value;
    this.props.onNameChange(name);
  }
  render() {
    return (
      <div id="task-status-bar">
        <div className="task-status-bar-component">
          <input type="text" onChange={this.handleNameChange} value={this.props.name} />
        </div>
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
      return (
        <div className="task-status-bar-button-group">
          <button type="button" className="btn btn-primary" onClick={this.props.onRunTaskClick}>
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

class SidePane extends React.Component {
  render() {
    return (
      <div id="task-editor-side-panel">
        <h1>Results and meta data entry</h1>
        <p>
          This side panel will contain two panes.
          One allowing meta data to be edited,
          one showing results.
        </p>
      </div>
    );
  }
}

exports.TaskEditor = (data) => {
  ReactDOM.render(
    <TaskEditor {...data} />,
    document.getElementById("task-view")
  );
};
