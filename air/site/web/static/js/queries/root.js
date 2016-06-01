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

  addResult(result) {
    this.setState({sessionResults: [result].concat(this.state.sessionResults)});
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
    $.ajax("/queries", {
      method: "POST",
      headers: {
        "X-CSRF-TOKEN": this.props.CSRFToken,
        "Content-Type": "application/json",
      },
      data: this.queryData(),
      success: (response) => { if (!response.success) { this.addError(response.reason); } },
      error: (error) => this.addError(error.statusText),
    });
  }

  addError(text) {
    this.addResult({statement: this.state.statement, error: text});
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
