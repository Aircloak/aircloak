import React from "react";
import ReactDOM from "react-dom";

import {CodeEditor} from "../code_editor";
import {Results} from "./results";
import {MenuButton} from "../menu";

class QueriesView extends React.Component {
  runQuery() { }

  render() {
    return (<div>
      <CodeEditor
        onRun={() => {}}
        onSave={() => {}}
        onChange={() => {}}
      />

      <MenuButton onClick={this.runQuery} isActive>Run</MenuButton>

      <Results {...this.props} />
    </div>);
  }
}

export default function renderQueriesView(data, elem) {
  ReactDOM.render(<QueriesView {...data} />, elem);
}
