import React from "react";
import ReactDOM from "react-dom";

import {CodeEditor} from "../code_editor";

const QueriesView = (_props) =>
  <CodeEditor
    onRun={() => {}}
    onSave={() => {}}
    onChange={() => {}}
  />;

export default function renderQueriesView(data, elem) {
  ReactDOM.render(<QueriesView {...data} />, elem);
}
