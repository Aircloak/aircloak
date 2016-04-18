import React from "react"
import { ResultsView } from "./results"
import { SettingsView } from "./settings"

export class SidePane extends React.Component {
  constructor(props) {
    super(props);

    // If there are results to be shown, indicating that
    // the task is functional, we show the results pane
    // upon loading the task. Otherwise, this might very
    // well be a new task, in which case the settings
    // pane is more useful.
    var activeTab = undefined;
    if (this.props.result != undefined) {
      activeTab = "Task result";
    } else {
      activeTab = "Settings";
    }

    this.state = {
      activeTab: activeTab
    }
    this.handleTabChange = this.handleTabChange.bind(this);
  }
  tabs() {
    return [
      {
        name: "Task result",
        component: <ResultsView {...this.props} result={this.props.result} />
      },
      {
        name: "Settings",
        component: <SettingsView {...this.props} />
      }
    ];
  }

  handleTabChange(tabName) {
    this.setState({activeTab: tabName});
  }

  activePane() {
    var activePane = this.tabs().find((tab) => tab.name == this.state.activeTab);
    return activePane.component;
  }

  render() {
    var tabs = this.tabs().map((tab) => {
      return <SelectionTab key={tab.name}
          name={tab.name} activeTab={this.state.activeTab}
          onClick={this.handleTabChange} />
    });
    return (
      <div id="task-editor-side-panel">
        <ul className="nav nav-tabs">
          {tabs}
        </ul>
        <div id="task-side-content">
          {this.activePane()}
        </div>
      </div>
    );
  }
}

class SelectionTab extends React.Component {
  constructor(props) {
    super(props);
    this.handleClick = this.handleClick.bind(this);
  }

  handleClick() {
    this.props.onClick(this.props.name);
  }

  render() {
    return (
      <li role="presentation" className={this.props.activeTab == this.props.name ? 'active' : ''}>
        <a href="#" onClick={this.handleClick}>{this.props.name}</a>
      </li>
    );
  }
}
