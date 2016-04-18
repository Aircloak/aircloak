import React from "react"
import ReactDOM from "react-dom"

// Imported components
import { CodeEditor } from "./code_editor"
import { ResultSocket } from "./results_socket"
import { SidePane } from "./sidepane"
import { StatusLine } from "./status_line"

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
      runningPercent: -1
    }
    // we keep a backup copy of the last saved
    // state of the task, in order to be able to
    // verify whether the task needs saving or not
    this.state.savedState = this.queryData();

    // Bind the handlers, so we can pass them, and have `this`
    // bound to the right context
    this.checkForUnsavedChanges = this.checkForUnsavedChanges.bind(this);
    this.conditionallySave = this.conditionallySave.bind(this);
    this.handleCodeChange = this.handleCodeChange.bind(this);
    this.handleNameChange = this.handleNameChange.bind(this);
    this.handleRunTask = this.handleRunTask.bind(this);
    this.isSaved = this.isSaved.bind(this);
    this.saveTask = this.saveTask.bind(this);
    this.updateTaskResult = this.updateTaskResult.bind(this);
    this.updateTaskRunningProgress = this.updateTaskRunningProgress.bind(this);

    // To prevent the user loosing changes, we ask whether
    // the page should be closed, if changes have been
    // made since the last time the task was saved.
    window.onbeforeunload = this.checkForUnsavedChanges;

    new ResultSocket(props.id, props.guardianToken)
      .start({
        joined: (resp) => {console.log("Joined channel for task updates")},
        failed_join: (resp) => {console.log("Failed to join channel for task updates");},
        progress: this.updateTaskRunningProgress,
        result: this.updateTaskResult
      });
  }


  // ----------------------------------------------------------------
  // Event handlers
  // ----------------------------------------------------------------

  handleNameChange(name) {
    this.setState({name: name});
  }

  handleCodeChange(query) {
    this.setState({query: query});
  }

  updateTaskRunningProgress(progress) {
    this.setState({runningPercent: progress});
  }

  updateTaskResult(result) {
    // We assume that the task is now complete, since we received a
    // result, and therefore update the progress too
    this.updateTaskRunningProgress(-1);
    this.setState({result: result});
  }


  // ----------------------------------------------------------------
  // Communication with air
  // ----------------------------------------------------------------

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


  // ----------------------------------------------------------------
  // Utility functions
  // ----------------------------------------------------------------

  conditionallySave() {
    if (!this.isSaved()) {
      this.saveTask()
    }
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

  isSaved() {
    // Hack to get object comparison in order to see whether or not the object is changed
    return (JSON.stringify(this.state.savedState) == JSON.stringify(this.queryData()));
  }

  checkForUnsavedChanges() {
    if (!this.isSaved()) {
      return "Your task has unsaved changes!";
    }
  }


  // ----------------------------------------------------------------
  // React callbacks
  // ----------------------------------------------------------------

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
            onSave={this.conditionallySave}
            onRun={this.handleRunTask} />
        <SidePane result={this.state.result} />
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
