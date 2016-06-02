import React from "react";

import {CodeEditor} from "../code_editor";

export const Error = (props) =>
  <div className="panel panel-danger">
    <div className="panel-heading" />
    <div className="panel-body">
      <CodeEditor
        onRun={() => {}}
        onSave={() => {}}
        onChange={() => {}}
        statement={props.statement}
        readOnly
      />

      <h4>Query failed</h4>
      <p>{props.error}</p>
    </div>
  </div>;

Error.propTypes = {
  statement: React.PropTypes.string,
  error: React.PropTypes.string,
};
