import React from "react"
import ReactDOM from "react-dom"
import Codemirror from "react-codemirror"

class TaskEditor extends React.Component {
  constructor(props) {
    super();

    this.state = {
      query: props.query,
      name: props.name,
      running_percent: -1,
      isSaved: true
    }

    // Bind the handlers, so we can pass them, and have `this`
    // bound to the right context
    this.handleNameChange = this.handleNameChange.bind(this);
    this.handleCodeChange = this.handleCodeChange.bind(this);
    this.handleRunTask = this.handleRunTask.bind(this);
    this.updateServer = this.updateServer.bind(this);

    // We throttle updates, so that we ensure we don't blast the server
    this.pending_save_request = false
    setInterval(this.updateServer, 1000);
  }
  handleNameChange(name) {
    this.setState({name: name});
    this.scheduleUpdate();
  }
  handleCodeChange(query) {
    this.setState({query: query});
    this.scheduleUpdate();
  }
  handleRunTask() {
    this.setState({running_percent: 0});
  }
  scheduleUpdate() {
    this.setState({isSaved: false});
    this.pending_save_request = true;
  }
  updateServer() {
    if (this.pending_save_request) {
      this.pending_save_request = false;
      $.ajax(`/tasks/${this.props.id}`, {
            context: this,
            method: "PUT",
            headers: {
              "X-CSRF-TOKEN": this.props.csrf_token
            },
            data: {
              task: {
                query: this.state.query,
                name: this.state.name
              }
            },
            success: (responseData, textStatus) => {
              if (!this.pending_save_request) {
                this.setState({isSaved: true});
              }
            },
            error: (jqXHR, textStatus) => {
              console.log(`Error: ${textStatus}`);
            }
        });
    }
  }
  render() {
    return (
      <div id="task-editor-container">
        <StatusLine
            running_percent={this.state.running_percent}
            name={this.state.name}
            isSaved={this.state.isSaved}
            onNameChange={this.handleNameChange}
            onRunTaskClick={this.handleRunTask} />
        <CodeEditor query={this.state.query} onChange={this.handleCodeChange} />
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
    var saveState = "";
    if (this.props.isSaved) {
      saveState = "Task saved";
    } else {
      saveState = "Saving...";
    }
    return (
      <div id="task-status-bar">
        <div className="task-status-bar-component">
          <input type="text" onChange={this.handleNameChange} value={this.props.name} />
        </div>
        <div id="save-state">
          {saveState}
        </div>
        <RunButton {...this.props} />
      </div>
    );
  }
}

class RunButton extends React.Component {
  render() {
    if (this.props.running_percent < 0) {
      return (
        <div id="task-run">
          <button type="button" className="btn btn-primary" onClick={this.props.onRunTaskClick}>
            Run task
          </button>
        </div>
      );
    } else {
      return (
        <div id="task-run">
          <button type="button" className="btn btn-primary disabled">
            Run task
          </button>
          <div className="progress">
            <div className="progress-bar" role="progressbar"
                aria-valuenow="{this.props.running_percent}" aria-valuemin="0"
                aria-valuemax="100" style={{width: this.props.running_percent + '%'}}>
              {this.props.running_percent}%
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

class CodeEditor extends React.Component {
  render() {
    var options = {
      lineNumbers: true,
      mode: "lua"
    };
    return (
          <Codemirror
              value={this.props.query}
              onChange={this.props.onChange}
              options={options} />
        );
  }
};

exports.TaskEditor = (data) => {
  ReactDOM.render(
    <TaskEditor {...data} />,
    document.getElementById("task-view")
  );
};
