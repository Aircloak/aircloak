// @flow

import React from "react";
import _ from "lodash";
import Mousetrap from "mousetrap";
import Channel from "phoenix";
import uuidv4 from "uuid/v4";

import {AuthContext} from "../authentication_provider";
import {CodeEditor} from "../code_editor";
import {CodeViewer} from "../code_viewer";
import {Results} from "./results";
import type {Result, PendingResult} from "./result";
import type {NumberFormat} from "../number_format";
import type {Selectable} from "../selectable_info/selectable";
import {FrontendSocket} from "../frontend_socket";
import {HistoryLoader} from "./history_loader";
import type {History} from "./history_loader";
import {Disconnected} from "../disconnected";
import {isFinished} from "./state";
import {startQuery, loadHistory} from "../request";
import {activateTooltips} from "../tooltips";

type Props = {
  userId: number,
  sessionId: string,
  socketToken: string,
  dataSourceName: string,
  dataSourceDescription: ?string,
  dataSourceStatus: string,
  selectables: Selectable[],
  lastQuery: {statement: string},
  pendingQueries: Result[],
  frontendSocket: FrontendSocket,
  numberFormat: NumberFormat,
  debugModeEnabled: boolean,
};

type State = {
  statement: string,
  sessionResults: Result[],
  history: History,
  connected: boolean,
  dataSourceStatus: string,
}

const upgradeRequired = 426;

const runQueryTimeout = 500; // ms

const recentResultsToShow = 5;

const historyPageSize = 10;

const emptyHistory = {
  before: "",
  loaded: false,
  loading: false,
};

export default class QueriesView extends React.PureComponent<Props, State> {
  constructor(props: Props) {
    super(props);

    this.state = {
      statement: this.initialStatement(),
      sessionResults: this.props.pendingQueries,
      connected: true,
      dataSourceStatus: this.props.dataSourceStatus,
      history: emptyHistory,
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
    this.initialStatement = this.initialStatement.bind(this);

    this.bindKeysWithoutEditorFocus();
    this.props.frontendSocket.joinDataSourceChannel(this.props.dataSourceName, {
      handleEvent: (event) => this.dataSourceStatusReceived(event),
    });
    this.channel = this.props.frontendSocket.joinUserQueriesChannel(this.props.userId, {
      handleEvent: (event) => this.resultReceived(event),
    });
    this.connectedInterval = setInterval(this.updateConnected, 1000 /* 1 second */);
  }

  connectedInterval: IntervalID;

  channel: Channel;

  initialStatement = () => (this.props.lastQuery ? this.props.lastQuery.statement : "")

  static contextType = AuthContext;

  componentWillUnmount = () => {
    clearInterval(this.connectedInterval);
  }

  updateConnected = () => {
    this.setState({connected: this.channel.isJoined()});
  }

  runEnabled = () => this.dataSourceAvailable() && this.state.connected

  dataSourceAvailable = () => this.state.dataSourceStatus !== "offline"

  setStatement = (statement: string) => {
    this.setState({statement});
  }

  setResults = (results: Result[]) => {
    let completed = 0;
    const recentResults = _.takeWhile(results, (result) => {
      if (isFinished(result.query_state)) { completed++; }
      return completed <= recentResultsToShow;
    });

    if (_.isEmpty(recentResults)) {
      this.setState({sessionResults: recentResults});
    } else {
      const history = _.assign({}, emptyHistory, {before: _.last(recentResults).inserted_at});
      this.setState({sessionResults: recentResults, history});
    }
  }

  replaceResult = (result: Result) => {
    const sessionResults = this.state.sessionResults.map((item) => {
      if (item.id === result.id) {
        return result;
      } else {
        return item;
      }
    });
    this.setResults(sessionResults);
  }

  resultReceived = (result: Result) => {
    if (this.shouldDisplayResult(result)) {
      this.replaceResult(result);
      if (result.query_state === "error") {
        this.parseResultError(result.error);
      }
    } else {
      // Ignore result
    }
  }

  dataSourceStatusReceived = (event: {status: string}) => {
    this.setState({dataSourceStatus: event.status});
  }

  shouldDisplayResult = (result: Result) => this.createdInThisSession(result) || this.alreadyDisplayed(result)

  createdInThisSession = (result: Result) => result.session_id === this.props.sessionId

  alreadyDisplayed = (result: Result) => _.some(this.state.sessionResults, (sessionResult) => sessionResult.id === result.id)

  addPendingResult = (queryId: string, statement: string) => {
    const pendingResult: PendingResult = {
      id: queryId,
      statement,
      query_state: "created",
      session_id: this.props.sessionId,
      private_permalink: null,
      public_permalink: null,
      inserted_at: null,
      data_source: {name: this.props.dataSourceName},
    };
    this.setResults([pendingResult].concat(this.state.sessionResults));
  }

  replacePendingResultWithError = (generatedTempId: string, statement: string, error: string) => {
    const errorResult = {
      query_state: "error",
      id: generatedTempId,
      statement,
      error,
      info: [],
      private_permalink: null,
      public_permalink: null,
      inserted_at: null,
      session_id: this.props.sessionId,
      data_source: {name: this.props.dataSourceName},
    };
    this.replaceResult(errorResult);
  }

  bindKeysWithoutEditorFocus = () => {
    Mousetrap.bind(["command+enter", "ctrl+enter"], this.runQuery);
  }

  queryData = (queryId: string) => JSON.stringify({
    query: {
      id: queryId,
      statement: this.state.statement,
      data_source_name: this.props.dataSourceName,
      session_id: this.props.sessionId,
    },
  })

  runQuery = () => {
    if (!this.runEnabled()) return;

    window.clearErrorLocation();

    const queryId = uuidv4();
    const {statement} = this.state;
    this.addPendingResult(queryId, statement);

    startQuery(this.queryData(queryId), this.context.authentication, {
      success: (response) => {
        if (!response.success) {
          this.replacePendingResultWithError(queryId, statement,
            `Error connecting to server. Reported reason: ${response.reason}.`);
        }
      },

      error: (error) => {
        this.replacePendingResultWithError(queryId, statement,
          `Error connecting to server. Reported reason: ${error.statusText}.`);
        if (error.status === upgradeRequired) { window.location.reload(); }
      },
    });
  }

  parseResultError = (error: string) => {
    if (!error) return;
    const matches = error.match(/at line (\d+), column (\d+)/i);
    if (!matches) return;
    const line = parseInt(matches[1], 10);
    const char = parseInt(matches[2], 10);
    window.showErrorLocation(line - 1, char - 1);
  }

  handleLoadHistory = () => {
    const {before} = this.state.history;
    const history = {
      before,
      loaded: false,
      loading: true,
    };
    this.setState({history});

    loadHistory(this.props.dataSourceName, before, this.context.authentication, {
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

  tableNames = () => this.props.selectables.map<string>((table) => table.id)

  columnNames = () => _.flatMap(this.props.selectables, (table) => table.columns.map<string>((column) => column.name))

  renderCodeEditorOrViewer = () => {
    if (this.runEnabled()) {
      return (
        <CodeEditor
          onRun={this.runQuery}
          onChange={this.setStatement}
          statement={this.initialStatement()}
          tableNames={this.tableNames()}
          columnNames={this.columnNames()}
        />
      );
    } else {
      return <CodeViewer statement={this.initialStatement()} />;
    }
  }

  renderButton = () => (
    <button
      className="btn btn-primary"
      onClick={this.runQuery}
      disabled={!this.runEnabled()}
      data-toggle="tooltip"
      data-placement="bottom"
      title="or press Ctrl + Enter"
    >
Run
    </button>
  )

  render = () => {
    activateTooltips();
    return (
      <>
        <Disconnected channel={this.channel} />

        <div id="sql-editor">
          {this.renderCodeEditorOrViewer()}
          {this.renderButton()}
        </div>

        <Results
          results={this.state.sessionResults}
          numberFormat={this.props.numberFormat}
          debugModeEnabled={this.props.debugModeEnabled}
          authentication={this.context.authentication}
        />

        <HistoryLoader history={this.state.history} handleLoadHistory={this.handleLoadHistory} />
      </>
    );
  }
}
