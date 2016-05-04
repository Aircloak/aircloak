import React from "react"
import ReactDOM from "react-dom"
import Mousetrap from "mousetrap"

// Imported components
import { CodeEditor } from "./code_editor"
import { ResultSocket } from "./results_socket"
import { SidePane } from "./sidepane"
import { MenuBar } from "./menubar"
import { SettingsModel } from "./settings"

class TaskEditor extends React.Component {
  constructor(props) {
    super();

    this.state = {
      // active state used in the query editor
      // and task name field
      query: props.query,
      settings: new SettingsModel({
            dataSources: props.data_sources,
            dataSourceToken: props.data_source_token,
            tables: new Set(props.tables),
            cloakId: props.cloak_id,
            taskName: props.name
          }),

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
    this.handleSettingsChange = this.handleSettingsChange.bind(this);
    this.handleRunTask = this.handleRunTask.bind(this);
    this.isSaved = this.isSaved.bind(this);
    this.canRun = this.canRun.bind(this);
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
        failed_join: (resp) => {console.error("Failed to join channel for task updates");},
        progress: this.updateTaskRunningProgress,
        result: this.updateTaskResult
      });

    // We trap save and run key-board shortcuts while the user is editing the task through
    // the code mirror editor itself. To avoid confusion, the same behaviour to be preserved also when
    // the user is not actively editing the task. To achieve this, we bind the key's here as well,
    // which ensures they are active all the time.
    Mousetrap.bind(['ctrl+s'], this.saveTask);
    Mousetrap.bind(['ctrl+r'], this.handleRunTask);
  }


  // ----------------------------------------------------------------
  // Event handlers
  // ----------------------------------------------------------------

  handleCodeChange(query) {
    this.setState({query: query});
  }

  handleSettingsChange(settings) {
    this.setState({settings: settings});
  }

  updateTaskRunningProgress(progress) {
    this.setState({runningPercent: progress});
  }

  updateTaskResult(result) {
    // We assume that the task is now complete, since we received a
    // result, and therefore update the progress too
    this.updateTaskRunningProgress(-1);
    this.setState({result: result});
    this.refs.SidePane.showResultTab()
  }


  // ----------------------------------------------------------------
  // Communication with air
  // ----------------------------------------------------------------

  handleRunTask() {
    this.updateTaskRunningProgress(0);
    $.ajax(`/tasks/${this.props.id}/run`, {
          context: this,
          method: "POST",
          headers: {
            "X-CSRF-TOKEN": this.props.CSRFToken,
            "Content-Type": "application/json"
          },
          data: this.queryData(),
          success: (responseData, textStatus) => {
            if (responseData.success)
              console.log("Task run scheduled...");
            else
              this.updateTaskResult({error: responseData.reason || "unknown error"});
          },
          error: (jqXHR, status, errorReason) => {
            console.error("Task run failed: ", errorReason);
            this.updateTaskResult({error: errorReason});
          }
        });
  }

  saveTask() {
    $.ajax(`/tasks/${this.props.id}`, {
          context: this,
          method: "PUT",
          headers: {
            "X-CSRF-TOKEN": this.props.CSRFToken,
            "Content-Type": "application/json"
          },
          data: this.queryData(),
          success: this.updateSavedState,
          error: (jqXHR, status, errorStatus) => {
            console.error("Task saving failed: ", errorReason);
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
    return JSON.stringify({
          task: {
            name: this.state.settings.taskName,
            query: this.state.query,
            data_source_token: this.state.settings.dataSourceToken,
            tables: Array.from(this.state.settings.tables)
          }
        });
  }

  isSaved() {
    return (this.state.savedState == this.queryData());
  }

  canRun() {
    return (this.state.settings.dataSourceToken != null && this.state.settings.tables.size > 0);
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
        <MenuBar
            runningPercent={this.state.runningPercent}
            isSavedCheck={this.isSaved}
            canRunCheck={this.canRun}
            onTaskSaveClick={this.saveTask}
            onRunTaskClick={this.handleRunTask} />
        <CodeEditor
            query={this.state.query}
            settings={this.state.settings}
            completions={this.props.completions}
            onChange={this.handleCodeChange}
            onSave={this.conditionallySave}
            onRun={this.handleRunTask} />
        <SidePane
            ref="SidePane"
            {...this.state}
            result={this.state.result}
            settings={this.state.settings}
            onSettingsChange={this.handleSettingsChange} />
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
