import React from "react";
import ReactDOM from "react-dom";
import Mousetrap from "mousetrap";
import $ from "jquery";

// Imported components
import {CodeEditor} from "./code_editor";
import {ResultSocket} from "./results_socket";
import {SidePane} from "./sidepane";
import {PaneView} from "./pane_view";
import {Menu, MenuButton, TaskProgress, PaneSelectButton, InfoBox} from "./menu";
import {SettingsModel, SettingsView} from "./settings";
import {ResultsView} from "./results";

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
        taskName: props.name,
      }),
      activeSidePane: null,

      // We keep some stats on whether or not
      // the task is running. This is used
      // throughout the interface to toggle
      // buttons and show progress bars
      runningPercent: -1,
    };
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
    this.handleHideSidePane = this.handleHideSidePane.bind(this);
    this.handleRunTask = this.handleRunTask.bind(this);
    this.hasChanges = this.hasChanges.bind(this);
    this.canRun = this.canRun.bind(this);
    this.infoBoxContent = this.infoBoxContent.bind(this);
    this.saveTask = this.saveTask.bind(this);
    this.taskIsRunning = this.taskIsRunning.bind(this);
    this.updateTaskResult = this.updateTaskResult.bind(this);
    this.updateTaskRunningProgress = this.updateTaskRunningProgress.bind(this);
    this.createActivePaneCheck = this.createActivePaneCheck.bind(this);
    this.activatePane = this.activatePane.bind(this);

    // To prevent the user loosing changes, we ask whether
    // the page should be closed, if changes have been
    // made since the last time the task was saved.
    window.onbeforeunload = this.checkForUnsavedChanges;

    new ResultSocket(props.id, props.guardianToken)
      .start({
        joined: (_resp) => console.log("Joined channel for task updates"),
        failed_join: (_resp) => console.error("Failed to join channel for task updates"),
        progress: this.updateTaskRunningProgress,
        result: this.updateTaskResult,
      });

    // We trap save and run key-board shortcuts while the user is editing the task through
    // the code mirror editor itself. To avoid confusion, the same behaviour to be preserved also when
    // the user is not actively editing the task. To achieve this, we bind the key's here as well,
    // which ensures they are active all the time.
    Mousetrap.bind(["ctrl+s"], this.saveTask);
    Mousetrap.bind(["ctrl+r"], this.handleRunTask);
  }


  // ----------------------------------------------------------------
  // Event handlers
  // ----------------------------------------------------------------

  handleCodeChange(query) {
    this.setState({query});
  }

  handleSettingsChange(settings) {
    this.setState({settings});
  }

  handleHideSidePane() {
    this.setState({activeSidePane: null});
  }

  updateTaskRunningProgress(runningPercent) {
    this.setState({runningPercent});
  }

  updateTaskResult(result) {
    // We assume that the task is now complete, since we received a
    // result, and therefore update the progress too
    this.updateTaskRunningProgress(-1);
    this.setState({result});
    this.setState({activeSidePane: "results"});
  }


  // ----------------------------------------------------------------
  // Communication with air
  // ----------------------------------------------------------------

  handleRunTask() {
    // We abort immediately if the task cannot be run
    if (! this.canRun()) {
      return;
    }
    this.updateTaskRunningProgress(0);
    $.ajax(`/tasks/${this.props.id}/run`, {
      context: this,
      method: "POST",
      headers: {
        "X-CSRF-TOKEN": this.props.CSRFToken,
        "Content-Type": "application/json",
      },
      data: this.queryData(),
      success: (responseData, _textStatus) => {
        if (responseData.success) {
          console.log("Task run scheduled...");
        } else {
          this.updateTaskResult({error: responseData.reason || "unknown error"});
        }
      },
      error: (jqXHR, status, errorReason) => {
        console.error("Task run failed: ", errorReason);
        this.updateTaskResult({error: errorReason});
      },
    });
  }

  saveTask() {
    $.ajax(`/tasks/${this.props.id}`, {
      context: this,
      method: "PUT",
      headers: {
        "X-CSRF-TOKEN": this.props.CSRFToken,
        "Content-Type": "application/json",
      },
      data: this.queryData(),
      success: this.updateSavedState,
      error: (jqXHR, status, errorStatus) => {
        console.error("Task saving failed: ", errorStatus);
      },
    });
  }


  // ----------------------------------------------------------------
  // Utility functions
  // ----------------------------------------------------------------

  conditionallySave() {
    if (this.hasChanges()) {
      this.saveTask();
    }
  }

  updateSavedState() {
    this.setState({savedState: this.queryData()});
  }

  queryData() {
    return JSON.stringify({
      task: {
        name: this.state.settings.taskName,
        query: this.state.query,
        data_source_token: this.state.settings.dataSourceToken,
        tables: Array.from(this.state.settings.tables),
      },
    });
  }

  hasChanges() {
    return (this.state.savedState !== this.queryData());
  }

  canRun() {
    return (
      this.state.settings.dataSourceToken != null &&
      this.state.settings.tables.size > 0 &&
      ! this.taskIsRunning() &&
      this.state.settings.selectedCloakOnline());
  }

  taskIsRunning() {
    return this.state.runningPercent !== -1;
  }

  infoBoxContent() {
    if (!this.state.settings.hasAssignedDataSource()) {
      return {
        message: "Your task needs a datasource before it can be run",
        action: this.activatePane("settings"),
      };
    }
    if (!this.state.settings.hasAssignedTables()) {
      return {
        message: "You need to select at least one table to run your task",
        action: this.activatePane("settings"),
      };
    }
    if (!this.state.settings.selectedCloakOnline()) {
      return {
        message: "The task cannot be run while the cloak is offline",
        action: this.activatePane("settings"),
      };
    }
    return null;
  }

  checkForUnsavedChanges() {
    if (this.hasChanges()) {
      return "Your task has unsaved changes!";
    }

    return null;
  }

  createActivePaneCheck(pane) {
    return (() => this.state.activeSidePane === pane);
  }

  activatePane(pane) {
    return (() => this.setState({activeSidePane: pane}));
  }


  // ----------------------------------------------------------------
  // React callbacks
  // ----------------------------------------------------------------

  render() {
    return (
      <div id="task-editor-container">
        <Menu>
          <PaneSelectButton
            onClick={this.activatePane("settings")}
            isActive={this.createActivePaneCheck("settings")}
          >
            Settings
          </PaneSelectButton>
          <PaneSelectButton
            onClick={this.activatePane("results")}
            isActive={this.createActivePaneCheck("results")}
          >
            Results
          </PaneSelectButton>
          <MenuButton onClick={this.saveTask} isActive={this.hasChanges}>Save task</MenuButton>
          <div>
            <MenuButton onClick={this.handleRunTask} isActive={this.canRun}>Run task</MenuButton>
            <TaskProgress {...this.state} taskIsRunning={this.taskIsRunning} />
            <InfoBox info={this.infoBoxContent()} />
          </div>
        </Menu>
        <div>
          <CodeEditor
            sidePaneHidden={this.createActivePaneCheck(null)}
            query={this.state.query}
            settings={this.state.settings}
            completions={this.props.completions}
            onChange={this.handleCodeChange}
            onSave={this.conditionallySave}
            onRun={this.handleRunTask}
          />
          <SidePane sidePaneHidden={this.createActivePaneCheck(null)}>
            <PaneView
              onHideClick={this.handleHideSidePane}
              isActive={this.createActivePaneCheck("results")}
            >
              <ResultsView {...this.state} />
            </PaneView>
            <PaneView
              onHideClick={this.handleHideSidePane}
              isActive={this.createActivePaneCheck("settings")}
            >
              <SettingsView {...this.state} onChange={this.handleSettingsChange} />
            </PaneView>
          </SidePane>
        </div>
      </div>
    );
  }
}

TaskEditor.propTypes = {
  id: React.PropTypes.string,
  query: React.PropTypes.string,
  data_sources: React.PropTypes.array,
  tables: React.PropTypes.array,
  completions: React.PropTypes.array,
  cloak_id: React.PropTypes.string,
  name: React.PropTypes.string,
  data_source_token: React.PropTypes.string,
  guardianToken: React.PropTypes.string,
  CSRFToken: React.PropTypes.string,
};

exports.renderTaskEditor = (data) => {
  ReactDOM.render(
    <TaskEditor {...data} />,
    document.getElementById("task-view")
  );
};
