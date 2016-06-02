import React from "react";

import {CodeEditor} from "../code_editor";

export const PendingResult = (props) =>
  <div className="panel panel-info">
    <div className="panel-heading">
      <div className="panel-title">Query executing</div>
    </div>
    <div className="panel-body">
      <CodeEditor
        onRun={() => {}}
        onSave={() => {}}
        onChange={() => {}}
        statement={props.statement}
        readOnly
      />
    </div>
  </div>;

PendingResult.propTypes = {
  statement: React.PropTypes.string,
};
