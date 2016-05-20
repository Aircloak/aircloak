import React from "react";
import ReactDOM from "react-dom";

import {CodeEditor} from "../code_editor";
import {Results} from "./results";

const QueriesView = (props) =>
  <div>
    <CodeEditor
      onRun={() => {}}
      onSave={() => {}}
      onChange={() => {}}
    />
    <Results {...props} />
  </div>;

export default function renderQueriesView(data, elem) {
  ReactDOM.render(<QueriesView {...data} />, elem);
}
