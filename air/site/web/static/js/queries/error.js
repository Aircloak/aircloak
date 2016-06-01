import React from "react";

import {CodeEditor} from "../code_editor";

export const Error = (props) =>
  <div className="panel panel-danger">
    <div className="panel-heading">
      <div className="panel-title">Failed query</div>
    </div>
    <div className="panel-body">
      <CodeEditor
        onRun={() => {}}
        onSave={() => {}}
        onChange={() => {}}
        statement={props.statement}
        readOnly
      />
      <div className="alert alert-danger">{props.error}</div>
    </div>
  </div>;

Error.propTypes = {
  statement: React.PropTypes.string,
  error: React.PropTypes.string,
};
