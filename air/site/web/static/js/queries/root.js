import React from "react";
import ReactDOM from "react-dom";
import $ from "jquery";
import Mousetrap from "mousetrap";

import {CodeEditor} from "../code_editor";
import {Results} from "./results";
import {DataSourceSelector} from "./data_source_selector";
import {MenuButton} from "../menu";
import {ResultSocket} from "../result_socket";
import {HistoryLoader} from "./history_loader";

class QueriesView extends React.Component {
  constructor(props) {
    super(props);

    this.state = {
      statement: this.props.lastQuery ? this.props.lastQuery.statement : "",
      dataSource: this.props.sources[0] ? this.props.sources[0].token : "",
      sessionResults: [],

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
    this.handleLoadRows = this.handleLoadRows.bind(this);
    this.handleLessRows = this.handleLessRows.bind(this);
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
        data_source_token: this.state.dataSource,
      },
    });
  }

  runQuery() {
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
    this.setState({historyLoading: true});
    $.ajax("/queries/load_history", {
      method: "GET",
      headers: {
        "X-CSRF-TOKEN": this.props.CSRFToken,
        "Content-Type": "application/json",
      },
      success: (response) => {
        const sessionResults = this.state.sessionResults;
        sessionResults.push.apply(sessionResults, response);
        this.setState({sessionResults, historyLoaded: true});
      },
      error: (error) => this.setStatement({historyLoading: false}),
    });
  }

  handleLoadRows(result) {
    this.replaceResult($.extend(result, {loadingData: true}));
    $.ajax(`/query/${result.id}`, {
      method: "GET",
      headers: {
        "X-CSRF-TOKEN": this.props.CSRFToken,
        "Content-Type": "application/json",
      },
      success: (completeResult) => {
        this.replaceResult(completeResult);
      },
      error: (error) => console.log("Failed at loading result"),
    });
  }

  handleLessRows(result) {
    const updateableResult = $.extend({}, result);
    updateableResult.rows = result.rows.slice(0, 10);
    this.replaceResult(updateableResult);
  }

  addError(statement, text) {
    const result = {statement, error: text};
    this.setResults([result].concat(this.state.sessionResults));
  }

  render() {
    return (<div>
      <div id="aql-editor">
        <h3>Query editor</h3>
        <CodeEditor
          onRun={this.runQuery}
          onSave={() => {}}
          onChange={this.setStatement}
          statement={this.state.statement}
        />

        <div className="right-align">
          <MenuButton onClick={this.runQuery} isActive>Run</MenuButton> or <kbd>Ctrl + Enter</kbd>
        </div>

        <DataSourceSelector
          sources={this.props.sources}
          onChange={this.setDataSource}
          selectedDataSource={this.state.dataSource}
        />

      </div>

      <Results
        results={this.state.sessionResults}
        handleLoadRows={this.handleLoadRows}
        handleLessRows={this.handleLessRows}
      />

      <HistoryLoader {...this.state} handleLoadHistory={this.handleLoadHistory} />
    </div>);
  }
}

export default function renderQueriesView(data, elem) {
  const socket = new ResultSocket(data.userId, data.guardianToken);
  ReactDOM.render(<QueriesView resultSocket={socket} {...data} />, elem);
}

QueriesView.propTypes = {
  sources: DataSourceSelector.propTypes.sources,
  lastQuery: React.PropTypes.shape({
    statement: React.PropTypes.string.isRequired,
  }),
  CSRFToken: React.PropTypes.string.isRequired,
  resultSocket: React.PropTypes.instanceOf(ResultSocket).isRequired,
};
