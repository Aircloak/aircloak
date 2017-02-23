// @flow

import React from "react";
import ReactDOM from "react-dom";
import $ from "jquery";
import _ from "lodash";
import Mousetrap from "mousetrap";

import {CodeEditor} from "../code_editor";
import {CodeViewer} from "../code_viewer";
import {Results} from "./results";
import type {Result} from "./result";
import type {Selectable} from "../selectable_info/selectable";
import {QuerySocket} from "../query_socket";
import {HistoryLoader} from "./history_loader";
import type {History} from "./history_loader";
import {isPending} from "./state";

type Props = {
  userId: number,
  sessionId: string,
  guardianToken: string,
  dataSourceId: number,
  dataSourceAvailable: boolean,
  selectables: Selectable[],
  lastQuery: {statement: string},
  CSRFToken: string,
  querySocket: QuerySocket,
};

class QueriesView extends React.Component {
  constructor(props: Props) {
    super(props);

    this.state = {
      statement: this.props.lastQuery ? this.props.lastQuery.statement : "",
      sessionResults: [],

      history: {
        before: "",
        loaded: false,
        loading: false,
      },
    };

    this.setStatement = this.setStatement.bind(this);
    this.runQuery = this.runQuery.bind(this);
    this.stopQuery = this.stopQuery.bind(this);
    this.queryData = this.queryData.bind(this);
    this.addResult = this.addResult.bind(this);
    this.resultReceived = this.resultReceived.bind(this);
    this.setResults = this.setResults.bind(this);
    this.handleLoadHistory = this.handleLoadHistory.bind(this);
    this.replaceResult = this.replaceResult.bind(this);
    this.columnNames = this.columnNames.bind(this);
    this.tableNames = this.tableNames.bind(this);

    this.bindKeysWithoutEditorFocus();
    this.props.querySocket.joinSessionChannel(props.sessionId, {
      handleEvent: this.resultReceived,
    });
  }

  state: {
    statement: string,
    sessionResults: Result[],
    history: History,
  }
  setStatement: () => void;
  runQuery: () => void;
  stopQuery: () => void;
  queryData: () => void;
  addResult: () => void;
  resultReceived: () => void;
  setResults: () => void;
  handleLoadHistory: () => void;
  replaceResult: () => void;
  columnNames: () => void;
  tableNames: () => void;

  setStatement(statement) {
    this.setState({statement});
  }

  setResults(results) {
    this.setState({sessionResults: results.slice(0, 5)});
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
    if (result.data_source_id === this.props.dataSourceId) {
      this.addResult(result, true /* replace */);
    } else {
      // Ignore result
    }
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
    Mousetrap.bind(["command+escape", "ctrl+escape"], this.stopQuery);
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

  isQueryPending() {
    return this.state.sessionResults.length > 0 && isPending(this.state.sessionResults[0].query_state);
  }

  runQuery() {
    if (! this.props.dataSourceAvailable) return;
    if (this.isQueryPending()) return;

    const statement = this.state.statement;
    $.ajax("/queries", {
      method: "POST",
      headers: {
        "X-CSRF-TOKEN": this.props.CSRFToken,
        "Content-Type": "application/json",
      },
      data: this.queryData(),
      success: (response) => {
        if (response.success) {
          const result = {
            statement,
            id: response.query_id,
            pendingResult: true,
          };
          this.addResult(result, false /* replace */);
        } else {
          this.addError(statement, `Error connecting to server. Reported reason: ${response.reason}.`);
        }
      },
      error: (error) => {
        this.addError(statement, `Error connecting to server. Reported reason: ${error.statusText}.`);
      },
    });
  }

  stopQuery() {
    if (! this.props.dataSourceAvailable) return;
    if (! this.isQueryPending()) return;

    const queryId = this.state.sessionResults[0].id;
    $.ajax("/queries/cancel", {
      method: "POST",
      headers: {
        "X-CSRF-TOKEN": this.props.CSRFToken,
        "Content-Type": "application/json",
      },
      data: JSON.stringify({id: queryId}),
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
    $.ajax(`/queries/load_history/${this.props.dataSourceId}?before=${before}`, {
      method: "GET",
      headers: {
        "X-CSRF-TOKEN": this.props.CSRFToken,
        "Content-Type": "application/json",
      },
      success: (response) => {
        const successHistory = (response.length < 10) ? {
          before: "",
          loaded: true,
          loading: false,
        } : {
          before: response[response.length - 1].inserted_at,
          loaded: false,
          loading: false,
        };
        const sessionResults = this.state.sessionResults;
        sessionResults.push.apply(sessionResults, response);
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
    const result = {statement, error: text};
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

  renderCodeEditorOrViewer() {
    if (this.props.dataSourceAvailable) {
      return (<CodeEditor
        onRun={this.runQuery}
        onStop={this.stopQuery}
        onChange={this.setStatement}
        statement={this.state.statement}
        tableNames={this.tableNames()}
        columnNames={this.columnNames()}
      />);
    } else {
      return <CodeViewer statement={this.state.statement} />;
    }
  }

  render() {
    let button;
    if (this.isQueryPending()) {
      button = (<div className="right-align">
        <button
          className="btn btn-small btn-warning"
          onClick={this.stopQuery}
        >Cancel</button>
        <span> or </span>
        <kbd>Ctrl + Escape</kbd>
      </div>);
    } else {
      button = (<div className="right-align">
        <button
          className="btn btn-primary"
          onClick={this.runQuery}
          disabled={!this.props.dataSourceAvailable}
        >Run</button>
        <span> or </span>
        <kbd>Ctrl + Enter</kbd>
      </div>);
    }
    return (<div>
      <div id="sql-editor">
        {this.renderCodeEditorOrViewer()}

        {button}
      </div>

      <Results results={this.state.sessionResults} />

      <HistoryLoader history={this.state.history} handleLoadHistory={this.handleLoadHistory} />
    </div>);
  }
}

export default function renderQueriesView(data: Props, elem: Node) {
  const socket = new QuerySocket(data.guardianToken);
  ReactDOM.render(<QueriesView querySocket={socket} {...data} />, elem);
}
