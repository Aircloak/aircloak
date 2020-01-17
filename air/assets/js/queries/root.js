// @flow

import React from "react";
import _ from "lodash";
import Mousetrap from "mousetrap";
import Channel from "phoenix";
import uuidv4 from "uuid/v4";

import {AuthContext} from "../authentication_provider";
import CodeEditor from "../code_editor";
import CodeViewer from "../code_viewer";
import Results from "./results";
import type {Result, PendingResult} from "./result";
import type {NumberFormat} from "../number_format";
import type {Selectable} from "../selectable_info/selectable";
import FrontendSocket from "../frontend_socket";
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

    const {
      userId, pendingQueries, dataSourceName, dataSourceStatus, frontendSocket,
    } = props;

    this.state = {
      statement: this.initialStatement(),
      sessionResults: pendingQueries,
      connected: true,
      dataSourceStatus,
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
    frontendSocket.joinDataSourceChannel(dataSourceName, {
      handleEvent: (event) => this.dataSourceStatusReceived(event),
    });
    this.channel = frontendSocket.joinUserQueriesChannel(userId, {
      handleEvent: (event) => this.resultReceived(event),
    });
    this.connectedInterval = setInterval(this.updateConnected, 1000 /* 1 second */);
  }

  // eslint-disable-next-line react/static-property-placement
  static contextType = AuthContext;

  connectedInterval: IntervalID;

  channel: Channel;

  initialStatement = () => {
    const {lastQuery} = this.props;
    return (lastQuery ? lastQuery.statement : "");
  }

  componentWillUnmount = () => {
    clearInterval(this.connectedInterval);
  }

  updateConnected = () => {
    this.setState({connected: this.channel.isJoined()});
  }

  runEnabled = () => {
    const {connected} = this.state;
    return this.dataSourceAvailable() && connected;
  }

  dataSourceAvailable = () => {
    const {dataSourceStatus} = this.state;
    return dataSourceStatus !== "offline";
  }

  setStatement = (statement: string) => {
    this.setState({statement});
  }

  setResults = (results: Result[]) => {
    let completed = 0;
    const recentResults = _.takeWhile(results, (result) => {
      if (isFinished(result.query_state)) { completed += 1; }
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
    const {sessionResults} = this.state;
    const processedSessionResults = sessionResults.map((item) => {
      if (item.id === result.id) {
        return result;
      } else {
        return item;
      }
    });
    this.setResults(processedSessionResults);
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

  createdInThisSession = (result: Result) => {
    const {sessionId} = this.props;
    return result.session_id === sessionId;
  }

  alreadyDisplayed = (result: Result) => {
    const {sessionResults} = this.state;
    return _.some(sessionResults, (sessionResult) => sessionResult.id === result.id);
  }

  addPendingResult = (queryId: string, statement: string) => {
    const {sessionId, dataSourceName} = this.props;
    const {sessionResults} = this.state;
    const pendingResult: PendingResult = {
      id: queryId,
      statement,
      query_state: "created",
      session_id: sessionId,
      private_permalink: null,
      public_permalink: null,
      inserted_at: null,
      data_source: {name: dataSourceName},
    };
    this.setResults([pendingResult].concat(sessionResults));
  }

  replacePendingResultWithError = (generatedTempId: string, statement: string, error: string) => {
    const {sessionId, dataSourceName} = this.props;
    const errorResult = {
      query_state: "error",
      id: generatedTempId,
      statement,
      error,
      info: [],
      private_permalink: null,
      public_permalink: null,
      inserted_at: null,
      session_id: sessionId,
      data_source: {name: dataSourceName},
    };
    this.replaceResult(errorResult);
  }

  bindKeysWithoutEditorFocus = () => {
    Mousetrap.bind(["command+enter", "ctrl+enter"], this.runQuery);
  }

  queryData = (queryId: string) => {
    const {dataSourceName, sessionId} = this.props;
    const {statement} = this.state;
    return JSON.stringify({
      query: {
        id: queryId,
        statement,
        data_source_name: dataSourceName,
        session_id: sessionId,
      },
    });
  }

  runQuery = () => {
    if (!this.runEnabled()) return;

    window.clearErrorLocation();

    const queryId = uuidv4();
    const {statement} = this.state;
    const {authentication} = this.context;

    this.addPendingResult(queryId, statement);

    startQuery(this.queryData(queryId), authentication, {
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
    const {history, sessionResults} = this.state;
    const {before} = history;
    const updatedHistory = {
      before,
      loaded: false,
      loading: true,
    };
    this.setState({history: updatedHistory});

    const {dataSourceName} = this.props;
    const {authentication} = this.context;
    loadHistory(dataSourceName, before, authentication, {
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
        const ammendedSessionResults = _.uniqBy(sessionResults.concat(response), "id");
        this.setState({sessionResults: ammendedSessionResults, history: successHistory});
      },

      // eslint-disable-next-line no-unused-vars
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

  tableNames = () => {
    const {selectables} = this.props;
    return selectables.map<string>((table) => table.id);
  }

  columnNames = () => {
    const {selectables} = this.props;
    return _.flatMap(selectables, (table) => table.columns.map<string>((column) => column.name));
  }

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
      type="button"
    >
      Run
    </button>
  )

  render = () => {
    activateTooltips();
    const {numberFormat, debugModeEnabled} = this.props;
    const {sessionResults, history} = this.state;
    const {authentication} = this.context;
    return (
      <>
        <Disconnected channel={this.channel} />

        <div id="sql-editor">
          {this.renderCodeEditorOrViewer()}
          {this.renderButton()}
        </div>

        <Results
          results={sessionResults}
          numberFormat={numberFormat}
          debugModeEnabled={debugModeEnabled}
          authentication={authentication}
        />

        <HistoryLoader history={history} handleLoadHistory={this.handleLoadHistory} />
      </>
    );
  }
}
