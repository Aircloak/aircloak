import React from "react";
import ReactDOM from "react-dom";
import $ from "jquery";
import Mousetrap from "mousetrap";

import {CodeEditor} from "../code_editor";
import {Results} from "./results";
import {MenuButton} from "../menu";
import {ResultSocket} from "../result_socket";
import {HistoryLoader} from "./history_loader";

class QueriesView extends React.Component {
  constructor(props) {
    super(props);

    this.state = {
      statement: this.props.lastQuery ? this.props.lastQuery.statement : "",
      sessionResults: [],

      history: {
        loading: false,
        loaded: false,
      },
      historyLoading: false,
      historyLoaded: false,
    };


    this.setStatement = this.setStatement.bind(this);
    this.setDataSource = this.setDataSource.bind(this);
    this.runQuery = this.runQuery.bind(this);
    this.queryData = this.queryData.bind(this);
    this.addResult = this.addResult.bind(this);
    this.setResults = this.setResults.bind(this);
    this.handleLoadHistory = this.handleLoadHistory.bind(this);
    this.replaceResult = this.replaceResult.bind(this);

    this.bindKeysWithoutEditorFocus();
    this.props.resultSocket.start({
      result: this.addResult,
    });
  }

  setStatement(statement) {
    this.setState({statement});
  }

  setDataSource(dataSource) {
    this.setState({dataSource});
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

  addResult(result, dontReplace = false) {
    const existingResult = this.state.sessionResults.find((item) => item.id === result.id);
    if (existingResult === undefined) {
      this.setResults([result].concat(this.state.sessionResults));
    } else {
      if (dontReplace) {
        // This guards against a race condition where the response from the cloak comes through the websocket
        // before we have had time to process the response from the AJAX runQuery call. What has happened if
        // we end up here, is that we already have a response, which is more up to date than the one we are
        // trying to add.
        return;
      }
      this.replaceResult(result);
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
      },
    });
  }

  runQuery() {
    if (! this.props.dataSourceAvailable) {
      return;
    }
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
          this.addResult(result, true /* dontReplace */);
        } else {
          this.addError(statement, response.reason);
        }
      },
      error: (error) => this.addError(statement, error.statusText),
    });
  }

  handleLoadHistory() {
    const history = {
      loaded: false,
      loading: true,
    };
    this.setState({history});
    $.ajax(`/queries/load_history/${this.props.dataSourceId}`, {
      method: "GET",
      headers: {
        "X-CSRF-TOKEN": this.props.CSRFToken,
        "Content-Type": "application/json",
      },
      success: (response) => {
        const successHistory = {
          loaded: true,
          loading: false,
        };
        const sessionResults = this.state.sessionResults;
        sessionResults.push.apply(sessionResults, response);
        this.setState({sessionResults, history: successHistory});
      },
      error: (_error) => {
        const errorHistory = {
          loaded: false,
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

  render() {
    return (<div>
      <div id="aql-editor">
        <CodeEditor
          onRun={this.runQuery}
          onSave={() => {}}
          onChange={this.setStatement}
          statement={this.state.statement}
          readOnly={!this.props.dataSourceAvailable}
        />

        <div className="right-align">
          <MenuButton onClick={this.runQuery} isActive={this.props.dataSourceAvailable}>Run</MenuButton>&nbsp;
          or <kbd>Ctrl + Enter</kbd>
        </div>
      </div>

      <Results results={this.state.sessionResults} />

      <HistoryLoader history={this.state.history} handleLoadHistory={this.handleLoadHistory} />
    </div>);
  }
}

export default function renderQueriesView(data, elem) {
  const socket = new ResultSocket(data.userId, data.guardianToken);
  ReactDOM.render(<QueriesView resultSocket={socket} {...data} />, elem);
}

QueriesView.propTypes = {
  dataSourceId: React.PropTypes.string.isRequired,
  dataSourceAvailable: React.PropTypes.bool.isRequired,
  lastQuery: React.PropTypes.shape({
    statement: React.PropTypes.string.isRequired,
  }),
  CSRFToken: React.PropTypes.string.isRequired,
  resultSocket: React.PropTypes.instanceOf(ResultSocket).isRequired,
};
