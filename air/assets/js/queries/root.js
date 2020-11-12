// @flow

import type { Node, Element } from "React";
import type { AuthContextType } from "../authentication_provider";
import React from "react";
import debounce from "lodash/debounce";
import takeWhile from "lodash/takeWhile";
import uniqBy from "lodash/uniqBy";
import Mousetrap from "mousetrap";
import type { Channel } from "phoenix";
import { v4 as uuidv4 } from "uuid";

import { AuthContext } from "../authentication_provider";
import CodeEditor from "../code_editor";
import type { Annotations } from "../code_editor";
import CodeViewer from "../code_viewer";
import Results from "./results";
import type { Result, PendingResult } from "./result";
import type { NumberFormat } from "../number_format";
import type { Selectable } from "../selectable_info/selectable";
import FrontendSocket from "../frontend_socket";
import { HistoryLoader } from "./history_loader";
import type { History } from "./history_loader";
import Disconnected from "../disconnected";
import { isFinished } from "./state";
import { startQuery, loadHistory, deleteQueryResult } from "../request";
import activateTooltips from "../tooltips";

type Props = {
  userId: number,
  sessionId: string,
  dataSourceName: string,
  dataSourceStatus: string,
  selectables: Selectable[],
  lastQuery: { statement: string },
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
  annotations: Annotations,
};

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
      userId,
      pendingQueries,
      dataSourceName,
      dataSourceStatus,
      frontendSocket,
    } = props;

    this.state = {
      statement: this.initialStatement(),
      sessionResults: pendingQueries,
      connected: true,
      dataSourceStatus,
      history: emptyHistory,
      annotations: "loading",
    };

    this.runQuery = debounce(this.runQuery.bind(this), runQueryTimeout, {
      leading: true,
      trailing: false,
    });

    this.bindKeysWithoutEditorFocus();
    frontendSocket.joinDataSourceChannel(dataSourceName, {
      handleEvent: (event) => this.dataSourceStatusReceived(event),
    });
    this.channel = frontendSocket.joinUserQueriesChannel(userId, {
      handleEvent: (event) => this.resultReceived(event),
    });
    this.typeCheckingChannel = frontendSocket.joinTypeCheckChannel(userId, {});

    this.connectedInterval = setInterval(
      this.updateConnected,
      1000 /* 1 second */
    );
  }

  componentDidMount() {
    this.setStatement(this.initialStatement());
  }

  // eslint-disable-next-line react/static-property-placement
  static contextType: React$Context<AuthContextType> = AuthContext;

  connectedInterval: IntervalID;

  channel: Channel;

  typeCheckingChannel: Channel;

  initialStatement: any | (() => string) = () => {
    const { lastQuery } = this.props;
    return lastQuery ? lastQuery.statement : "";
  };

  componentWillUnmount: () => void = () => {
    clearInterval(this.connectedInterval);
  };

  updateConnected: any | (() => void) = () => {
    this.setState({ connected: this.channel.isJoined() });
  };

  runEnabled: any | (() => boolean) = () => {
    const { connected } = this.state;
    return this.dataSourceAvailable() && connected;
  };

  dataSourceAvailable: () => boolean = () => {
    const { dataSourceStatus } = this.state;
    return dataSourceStatus !== "offline";
  };

  requestTypeCheck: (string) => void = debounce((statement) => {
    this.typeCheckingChannel
      .push("type_check", {
        query: statement,
        data_source: this.props.dataSourceName,
      })
      .receive("ok", ({ result }) => {
        this.setState({
          annotations: result.data,
        });
      });
  }, 100);

  setStatement: (statement: string) => void = (statement) => {
    this.requestTypeCheck(statement);
    this.setState({ statement, annotations: "loading" });
  };

  setResults: any | ((results: Array<Result>) => void) = (
    results: Result[]
  ) => {
    let completed = 0;
    const recentResults = takeWhile(results, (result) => {
      if (isFinished(result.query_state)) {
        completed += 1;
      }
      return completed <= recentResultsToShow;
    });

    if (recentResults.length === 0) {
      this.setState({ sessionResults: recentResults });
    } else {
      const history = Object.assign({}, emptyHistory, {
        before: recentResults[recentResults.length - 1].inserted_at,
      });
      this.setState({ sessionResults: recentResults, history });
    }
  };

  replaceResult: any | ((result: Result) => void) = (result: Result) => {
    const { sessionResults } = this.state;
    const processedSessionResults = sessionResults.map((item) => {
      if (item.id === result.id) {
        return result;
      } else {
        return item;
      }
    });
    this.setResults(processedSessionResults);
  };

  resultReceived: (result: Result) => void = (result: Result) => {
    if (this.shouldDisplayResult(result)) {
      this.replaceResult(result);
      if (result.query_state === "error") {
        this.parseResultError(result.error);
      }
    } else {
      // Ignore result
    }
  };

  dataSourceStatusReceived: (event: { status: string, ... }) => void = (event: {
    status: string,
  }) => {
    this.setState({ dataSourceStatus: event.status });
  };

  shouldDisplayResult: (result: Result) => boolean = (result: Result) =>
    this.createdInThisSession(result) || this.alreadyDisplayed(result);

  createdInThisSession: (result: Result) => boolean = (result: Result) => {
    const { sessionId } = this.props;
    return result.session_id === sessionId;
  };

  alreadyDisplayed: (result: Result) => boolean = (result: Result) => {
    const { sessionResults } = this.state;
    return sessionResults.some(
      (sessionResult) => sessionResult.id === result.id
    );
  };

  deleteResult: (queryId: string) => void = (queryId: string) => {
    if (window.confirm("Do you want to permanently delete this result?")) {
      deleteQueryResult(queryId, this.context.authentication);
      this.setState((state) => ({
        sessionResults: state.sessionResults.filter((r) => r.id !== queryId),
      }));
    }
  };

  addPendingResult: (queryId: string, statement: string) => void = (
    queryId: string,
    statement: string
  ) => {
    const { sessionId, dataSourceName } = this.props;
    const { sessionResults } = this.state;
    const pendingResult: PendingResult = {
      id: queryId,
      statement,
      query_state: "created",
      session_id: sessionId,
      private_permalink: null,
      public_permalink: null,
      inserted_at: Date.now(),
      data_source: { name: dataSourceName },
    };
    this.setResults([pendingResult].concat(sessionResults));
  };

  replacePendingResultWithError: (
    generatedTempId: string,
    statement: string,
    error: string
  ) => void = (generatedTempId: string, statement: string, error: string) => {
    const { sessionId, dataSourceName } = this.props;
    const errorResult = {
      query_state: "error",
      id: generatedTempId,
      statement,
      error,
      info: [],
      private_permalink: null,
      public_permalink: null,
      inserted_at: Date.now(),
      session_id: sessionId,
      data_source: { name: dataSourceName },
    };
    this.replaceResult(errorResult);
  };

  bindKeysWithoutEditorFocus: () => void = () => {
    Mousetrap.bind(["command+enter", "ctrl+enter"], this.runQuery);
  };

  queryData: any | ((queryId: string) => string) = (queryId: string) => {
    const { dataSourceName, sessionId } = this.props;
    const { statement } = this.state;
    return JSON.stringify({
      query: {
        id: queryId,
        statement,
        data_source_name: dataSourceName,
        session_id: sessionId,
      },
    });
  };

  runQuery: any | (() => void) = () => {
    if (!this.runEnabled()) return;

    window.clearErrorLocation();

    const queryId = uuidv4();
    const { statement } = this.state;
    const { authentication } = this.context;

    this.addPendingResult(queryId, statement);

    startQuery(this.queryData(queryId), authentication, {
      success: (response) => {
        if (!response.success) {
          this.replacePendingResultWithError(
            queryId,
            statement,
            `Error connecting to server. Reported reason: ${response.reason}.`
          );
        }
      },

      error: (error) => {
        this.replacePendingResultWithError(
          queryId,
          statement,
          `Error connecting to server. Reported reason: ${error.statusText}.`
        );
        if (error.status === upgradeRequired) {
          window.location.reload();
        }
      },
    });
  };

  parseResultError: (error: string) => void = (error: string) => {
    if (!error) return;
    const matches = error.match(/at line (\d+), column (\d+)/i);
    if (!matches) return;
    const line = parseInt(matches[1], 10);
    const char = parseInt(matches[2], 10);
    window.showErrorLocation(line - 1, char - 1);
  };

  handleLoadHistory: any | (() => void) = () => {
    this.setState((state) => ({
      history: { before: state.history.before, loaded: false, loading: true },
    }));

    const { dataSourceName } = this.props;
    const { authentication } = this.context;
    loadHistory(dataSourceName, this.state.history.before, authentication, {
      success: (response) => {
        const successHistory =
          response.length < historyPageSize
            ? {
                before: "",
                loaded: true,
                loading: false,
              }
            : {
                before: response[response.length - 1].inserted_at,
                loaded: false,
                loading: false,
              };
        this.setState((state) => ({
          sessionResults: uniqBy(state.sessionResults.concat(response), "id"),
          history: successHistory,
        }));
      },

      // eslint-disable-next-line no-unused-vars
      error: (_error) => {
        const errorHistory = {
          before: "",
          loaded: true,
          loading: false,
          error: true,
        };
        this.setState({ history: errorHistory });
      },
    });
  };

  tableNames: any | (() => Array<string>) = () => {
    const { selectables } = this.props;
    return selectables.map<string>((table) => table.id);
  };

  columnNames: any | (() => Array<string>) = (): Array<string> => {
    const { selectables } = this.props;
    return selectables.flatMap((table) =>
      table.columns.map<string>((column) => column.name)
    );
  };

  renderCodeEditorOrViewer: () => Node = () => {
    if (this.runEnabled()) {
      return (
        <CodeEditor
          onRun={this.runQuery}
          onChange={this.setStatement}
          statement={this.state.statement}
          tableNames={this.tableNames()}
          columnNames={this.columnNames()}
          annotations={this.state.annotations}
        />
      );
    } else {
      return <CodeViewer statement={this.state.statement} />;
    }
  };

  renderButton: () => Element<"div"> = () => (
    <div>
      <button
        className="btn btn-primary"
        onClick={this.runQuery}
        disabled={!this.runEnabled()}
        data-toggle="tooltip"
        data-placement="left"
        title="or press Ctrl + Enter"
        type="button"
      >
        Run
      </button>
      <button
        type="button"
        className="btn d-block d-md-none"
        data-toggle="collapse"
        data-target="#sidebar"
        aria-expanded="false"
      >
        <i className="fas fa-list" aria-label="Show sidebar"></i>
      </button>
    </div>
  );

  render: () => Node = () => {
    activateTooltips();
    const { numberFormat, debugModeEnabled } = this.props;
    const { sessionResults, history } = this.state;
    const { authentication } = this.context;
    return (
      <>
        <Disconnected channel={this.channel} />

        <div id="sql-editor">
          <div className="scroller">
            {this.renderCodeEditorOrViewer()}
            {this.renderButton()}
          </div>
        </div>

        <Results
          results={sessionResults}
          numberFormat={numberFormat}
          debugModeEnabled={debugModeEnabled}
          authentication={authentication}
          onDeleteClick={this.deleteResult}
        />

        <HistoryLoader
          history={history}
          handleLoadHistory={this.handleLoadHistory}
        />
      </>
    );
  };
}
