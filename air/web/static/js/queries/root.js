// @flow

import React from "react";
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
import type {QueryData} from "../request";

type Props = {
  userId: number,
  sessionId: string,
  guardianToken: string,
  dataSourceId: number,
  dataSourceName: string,
  dataSourceStatus: string,
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
  queryData: () => QueryData;
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
    return this.dataSourceAvailable() && this.state.connected;
  }

  dataSourceAvailable() {
    return this.state.dataSourceStatus !== "offline";
  }

  setStatement(statement: string) {
    this.setState({statement});
  }

  setResults(results: Result[]) {
    let completed = 0;
    const recentResults = _.takeWhile(results, (result) => {
      if (isFinished(result.query_state)) { completed++; }
      return completed <= recentResultsToShow;
    });
    this.setState({sessionResults: recentResults});
  }

  replaceResult(result: Result) {
    const sessionResults = this.state.sessionResults.map((item) => {
      if (item.id === result.id) {
        return result;
      } else {
        return item;
      }
    });
    this.setResults(sessionResults);
  }

  resultReceived(result: Result) {
    if (this.shouldDisplayResult(result)) {
      this.replaceResult(result);
    } else {
      // Ignore result
    }
  }

  dataSourceStatusReceived(event: {status: string}) {
    this.setState({dataSourceStatus: event.status});
  }

  shouldDisplayResult(result: Result) {
    return this.createdInThisSession(result) || this.alreadyDisplayed(result);
  }

  createdInThisSession(result: Result) {
    return result.session_id === this.props.sessionId;
  }

  alreadyDisplayed(result: Result) {
    return _.some(this.state.sessionResults, (sessionResult) => sessionResult.id === result.id);
  }

  updateResultId(generatedTempId: string, serverSideId: string) {
    const sessionResults = _.chain(this.state.sessionResults).
      cloneDeep().
      map((item) => {
        if (item.id === generatedTempId) {
          const newItem = _.cloneDeep(item);
          newItem.id = serverSideId;
          return newItem;
        }
        return item;
      }).
      value();
    this.setResults(sessionResults);
  }

  addPendingResult(statement: string) {
    const generatedTempId = (Math.random() * 1e32).toString(36);

    const pendingResult = {
      statement,
      id: generatedTempId,
      query_state: "started",
    };
    this.setResults([pendingResult].concat(this.state.sessionResults));

    return generatedTempId;
  }

  replacePendingResultWithError(generatedTempId: string, statement: string, error: string) {
    const errorResult = {
      query_state: "error",
      id: generatedTempId,
      statement,
      error,
      info: [],
    };
    this.replaceResult(errorResult);
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
    const tempId = this.addPendingResult(statement);

    startQuery(this.queryData(), this.context.authentication, {
      success: (response) => {
        if (response.success) {
          this.updateResultId(tempId, response.query_id);
        } else {
          this.replacePendingResultWithError(tempId, statement,
            `Error connecting to server. Reported reason: ${response.reason}.`);
        }
      },

      error: (error) => {
        this.replacePendingResultWithError(tempId, statement,
          `Error connecting to server. Reported reason: ${error.statusText}.`);
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

  addError(statement: string, text: string) {
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
