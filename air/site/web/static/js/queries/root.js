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
      query: this.props.results[0] ? this.props.results[0].query : "",
      dataSource: this.props.sources[0] ? this.props.sources[0].token : "",
      sessionResults: [],
    };

    this.setQuery = this.setQuery.bind(this);
    this.setDataSource = this.setDataSource.bind(this);
    this.runQuery = this.runQuery.bind(this);
    this.queryData = this.queryData.bind(this);
    this.addResult = this.addResult.bind(this);

    this.bindKeysWithoutEditorFocus();
    this.props.resultSocket.start({
      result: this.addResult,
    });
  }

  setQuery(query) {
    this.setState({query});
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
      task: {
        query: this.state.query,
        data_source_token: this.state.dataSource,
      },
    });
  }

  runQuery() {
    $.ajax("/tasks/run", {
      method: "POST",
      context: this,
      headers: {
        "X-CSRF-TOKEN": this.props.CSRFToken,
        "Content-Type": "application/json",
      },
      data: this.queryData(),
      error: (error) => this.addResult({query: this.state.query, error: error.statusText}),
    });
  }

  render() {
    return (<div>
      <CodeEditor
        onRun={this.runQuery}
        onSave={() => {}}
        onChange={this.setQuery}
        query={this.state.query}
      />

      <div className="query-menu">
        <MenuButton onClick={this.runQuery} isActive>Run</MenuButton>
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
