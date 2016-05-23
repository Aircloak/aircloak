import React from "react";
import ReactDOM from "react-dom";
import $ from "jquery";

import {CodeEditor} from "../code_editor";
import {Results} from "./results";
import {DataSourceSelector} from "./data_source_selector";
import {MenuButton} from "../menu";

class QueriesView extends React.Component {
  constructor(props) {
    super(props);

    this.state = {
      query: this.props.results[0] ? this.props.results[0].query : "",
      dataSource: this.props.sources[0] ? this.props.sources[0].token : "",
    };

    this.setQuery = this.setQuery.bind(this);
    this.setDataSource = this.setDataSource.bind(this);
    this.runQuery = this.runQuery.bind(this);
    this.queryData = this.queryData.bind(this);
  }

  setQuery(query) {
    this.setState({query});
  }

  setDataSource(dataSource) {
    this.setState({dataSource});
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
      headers: {
        "X-CSRF-TOKEN": this.props.CSRFToken,
        "Content-Type": "application/json",
      },
      data: this.queryData(),
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

      <Results {...this.props} />
    </div>);
  }
}

export default function renderQueriesView(data, elem) {
  ReactDOM.render(<QueriesView {...data} />, elem);
}

QueriesView.propTypes = {
  sources: DataSourceSelector.propTypes.sources,
  results: Results.propTypes.results,
  CSRFToken: React.PropTypes.string.isRequired,
};
