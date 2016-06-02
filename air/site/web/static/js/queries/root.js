import React from "react";
import ReactDOM from "react-dom";
import $ from "jquery";
import Mousetrap from "mousetrap";

import {CodeEditor} from "../code_editor";
import {Results} from "./results";
import {DataSourceSelector} from "./data_source_selector";
import {MenuButton} from "../menu";
import {ResultSocket} from "../result_socket";

class QueriesView extends React.Component {
  constructor(props) {
    super(props);

    this.state = {
      statement: this.props.results[0] ? this.props.results[0].statement : "",
      dataSource: this.props.sources[0] ? this.props.sources[0].token : "",
      sessionResults: [],
    };

    this.setStatement = this.setStatement.bind(this);
    this.setDataSource = this.setDataSource.bind(this);
    this.runQuery = this.runQuery.bind(this);
    this.queryData = this.queryData.bind(this);
    this.addResult = this.addResult.bind(this);

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

  addResult(result, dontReplace = false) {
    const existingResult = this.state.sessionResults.find((item) => item.id === result.id);
    if (existingResult === undefined) {
      this.setState({sessionResults: [result].concat(this.state.sessionResults)});
    } else {
      if (dontReplace) {
        // This guards against a race condition where the response from the cloak comes through the websocket
        // before we have had time to process the response from the AJAX runQuery call. What has happened if
        // we end up here, is that we already have a response, which is more up to date than the one we are
        // trying to add.
        return;
      }
      const sessionResults = this.state.sessionResults.map((item) => {
        if (item.id === result.id) {
          return result;
        } else {
          return item;
        }
      });
      this.setState({sessionResults});
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

  addError(statement, text) {
    const result = {statement, error: text};
    this.setState({sessionResults: [result].concat(this.state.sessionResults)});
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

      <Results results={this.state.sessionResults.concat(this.props.results)} />
    </div>);
  }
}

export default function renderQueriesView(data, elem) {
  const socket = new ResultSocket(data.userId, data.guardianToken);
  ReactDOM.render(<QueriesView resultSocket={socket} {...data} />, elem);
}

QueriesView.propTypes = {
  sources: DataSourceSelector.propTypes.sources,
  results: Results.propTypes.results,
  CSRFToken: React.PropTypes.string.isRequired,
  resultSocket: React.PropTypes.instanceOf(ResultSocket).isRequired,
};
