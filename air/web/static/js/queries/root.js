// @flow

import React from "react";
import $ from "jquery";
import _ from "lodash";
import Mousetrap from "mousetrap";
import Channel from "phoenix";

import {CodeEditor} from "../code_editor";
import {CodeViewer} from "../code_viewer";
import {Results} from "./results";
import type {Result} from "./result";
import type {Selectable} from "../selectable_info/selectable";
import {FrontendSocket} from "../frontend_socket";
import {HistoryLoader} from "./history_loader";
import type {History} from "./history_loader";
import {Disconnected} from "../disconnected";
import {isFinished} from "./state";
import {startQuery, loadHistory} from "../request";

type Props = {
  userId: number,
  sessionId: string,
  guardianToken: string,
  dataSourceId: number,
  dataSourceName: string,
  dataSourceStatus: string,
  dataSourceAvailable: boolean,
  selectables: Selectable[],
  lastQuery: {statement: string},
  pendingQueries: Result[],
  frontendSocket: FrontendSocket,
};

const upgradeRequired = 426;

const runQueryTimeout = 500; // ms

const recentResultsToShow = 5;

const historyPageSize = 10;

export default class QueriesView extends React.Component {
  constructor(props: Props) {
    super(props);

    this.state = {
      statement: this.props.lastQuery ? this.props.lastQuery.statement : "",
      sessionResults: this.props.pendingQueries,
      connected: true,
      dataSourceStatus: this.props.dataSourceStatus,

      history: {
        before: "",
        loaded: false,
        loading: false,
      },
    };

    this.setStatement = this.setStatement.bind(this);
    this.runQuery = _.debounce(this.runQuery.bind(this), runQueryTimeout, {leading: true, trailing: false});
    this.queryData = this.queryData.bind(this);
    this.addResult = this.addResult.bind(this);
    this.setResults = this.setResults.bind(this);
    this.handleLoadHistory = this.handleLoadHistory.bind(this);
    this.replaceResult = this.replaceResult.bind(this);
    this.columnNames = this.columnNames.bind(this);
    this.tableNames = this.tableNames.bind(this);
    this.runEnabled = this.runEnabled.bind(this);
    this.updateConnected = this.updateConnected.bind(this);

    this.bindKeysWithoutEditorFocus();
    this.props.frontendSocket.joinDataSourceChannel(this.props.dataSourceId, {
      handleEvent: (event) => this.dataSourceStatusReceived(event),
    });
    this.channel = this.props.frontendSocket.joinUserQueriesChannel(this.props.userId, {
      handleEvent: (event) => this.resultReceived(event),
    });
    this.connectedInterval = setInterval(this.updateConnected, 1000 /* 1 second */);
  }

  state: {
    statement: string,
    sessionResults: Result[],
    history: History,
    connected: boolean,
    dataSourceStatus: string,
  }
  channel: Channel;
  connectedInterval: number;

  setStatement: () => void;
  runQuery: () => void;
  queryData: () => void;
  addResult: () => void;
  setResults: () => void;
  handleLoadHistory: () => void;
  replaceResult: () => void;
  columnNames: () => void;
  tableNames: () => void;
  updateConnected: () => void;

  runEnabled: () => boolean;

  componentWillUnmount() {
    clearInterval(this.connectedInterval);
  }

  updateConnected() {
    this.setState({connected: this.channel.isJoined()});
  }

  runEnabled() {
    return this.props.dataSourceAvailable && this.state.connected;
  }

  setStatement(statement) {
    this.setState({statement});
  }

  setResults(results) {
    let completed = 0;
    const recentResults = _.takeWhile(results, (result) => {
      if (isFinished(result.query_state)) { completed++; }
      return completed <= recentResultsToShow;
    });
    this.setState({sessionResults: recentResults});
  }

  replaceResult(result) {
    const sessionResults = this.state.sessionResults.map((item) => {
      if (item.id === result.id) {
        return result;
      } else {
        return item;
      }
    });
    this.setResults(sessionResults);
  }

  resultReceived(result) {
    if (this.shouldDisplayResult(result)) {
      this.addResult(result, true /* replace */);
    } else {
      // Ignore result
    }
  }

  dataSourceStatusReceived({status}) {
    this.setState({dataSourceStatus: status});
  }

  shouldDisplayResult(result) {
    return this.createdInThisSession(result) || this.alreadyDisplayed(result);
  }

  createdInThisSession(result) {
    return result.session_id === this.props.sessionId;
  }

  alreadyDisplayed(result) {
    return _.some(this.state.sessionResults, (sessionResult) => sessionResult.id === result.id);
  }

  addResult(result, replace = true) {
    const existingResult = _.find(this.state.sessionResults, (item) => item.id === result.id);
    if (existingResult === undefined) {
      this.setResults([result].concat(this.state.sessionResults));
    } else if (replace) {
      // This guards against a race condition where the response from the cloak comes through the websocket
      // before we have had time to process the response from the AJAX runQuery call.
      this.replaceResult(result);
    } else {
      // Ignore. What has happened if we end up here, is that we already have a response, which is more up to
      // date than the one we are trying to add.
    }
  }

  bindKeysWithoutEditorFocus() {
    Mousetrap.bind(["command+enter", "ctrl+enter"], this.runQuery);
  }

  queryData() {
    return JSON.stringify({
      query: {
        statement: this.state.statement,
        data_source_id: this.props.dataSourceId,
        session_id: this.props.sessionId,
      },
    });
  }

  runQuery() {
    if (! this.runEnabled()) return;

    const statement = this.state.statement;

    startQuery(this.queryData(), this.context.authentication, {
      success: (response) => {
        if (response.success) {
          const result = {
            statement,
            id: response.query_id,
            query_state: "started",
          };
          this.addResult(result, false /* replace */);
        } else {
          this.addError(statement, `Error connecting to server. Reported reason: ${response.reason}.`);
        }
      },

      error: (error) => {
        this.addError(statement, `Error connecting to server. Reported reason: ${error.statusText}.`);
        if (error.status === upgradeRequired) { window.location.reload(); }
      },
    });
  }

  handleLoadHistory() {
    const before = this.state.history.before;
    const history = {
      before,
      loaded: false,
      loading: true,
    };
    this.setState({history});

    loadHistory(this.props.dataSourceId, before, this.context.authentication, {
      success: (response) => {
        const successHistory = (response.length < historyPageSize) ? {
          before: "",
          loaded: true,
          loading: false,
        } : {
          before: response[response.length - 1].inserted_at,
          loaded: false,
          loading: false,
        };
        const sessionResults = _.uniqBy(this.state.sessionResults.concat(response), "id");
        this.setState({sessionResults, history: successHistory});
      },

      error: (_error) => {
        const errorHistory = {
          before: "",
          loaded: true,
          loading: false,
          error: true,
        };
        this.setState({history: errorHistory});
      },
    });
  }

  addError(statement, text) {
    const result = {statement, query_state: "error", error: text};
    this.setResults([result].concat(this.state.sessionResults));
  }

  tableNames() {
    return this.props.selectables.map((table) => table.id);
  }

  columnNames() {
    return _.flatMap(this.props.selectables, (table) =>
      table.columns.map((column) => column.name)
    );
  }

  renderAvailabilityLabel() {
    switch (this.state.dataSourceStatus) {
      case "online": return <span className="label label-success">Online</span>;
      case "offline": return <span className="label label-danger">Offline</span>;
      default: return <span className="label label-warning">Broken</span>;
    }
  }

  renderCodeEditorOrViewer() {
    if (this.runEnabled()) {
      return (<CodeEditor
        onRun={this.runQuery}
        onChange={this.setStatement}
        statement={this.state.statement}
        tableNames={this.tableNames()}
        columnNames={this.columnNames()}
      />);
    } else {
      return <CodeViewer statement={this.state.statement} />;
    }
  }

  renderButton() {
    return (<div className="right-align">
      <button
        className="btn btn-primary"
        onClick={this.runQuery}
        disabled={!this.runEnabled()}
      >Run</button>
      <span> or </span>
      <kbd>Ctrl + Enter</kbd>
    </div>);
  }

  render() {
    return (<div>
      <h2>
        {this.props.dataSourceName}
        &nbsp;
        {this.renderAvailabilityLabel()}
      </h2>

      <Disconnected channel={this.channel} />

      <div id="sql-editor">
        {this.renderCodeEditorOrViewer()}
        {this.renderButton()}
      </div>

      <Results results={this.state.sessionResults} />

      <HistoryLoader history={this.state.history} handleLoadHistory={this.handleLoadHistory} />
    </div>);
  }
}

QueriesView.contextTypes = {
  authentication: React.PropTypes.object.isRequired,
};
