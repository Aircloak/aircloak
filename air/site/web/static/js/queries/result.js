import React from "react";

import {CodeEditor} from "../code_editor";

export const Result = (props) =>
  <div className="panel panel-success">
    <div className="panel-heading">
      <div className="panel-title">Completed query</div>
    </div>
    <div className="panel-body">
      <CodeEditor
        onRun={() => {}}
        onSave={() => {}}
        onChange={() => {}}
        statement={props.statement}
        readOnly
      />
      <table className="table table-striped table-hover">
        <thead>
          <tr>
            {props.columns.map((column) =>
              <th key={column}>{column}</th>
            )}
          </tr>
        </thead>

        <tbody>
          {props.rows.
            map((row, i) =>
              <tr key={i}>
                {row.map((value, j) => <td key={j}>{value}</td>)}
              </tr>
            )
          }
        </tbody>
      </table>
      <div className="row-count">
        {props.rows.length} rows after anonymization
      </div>
    </div>
  </div>;

Result.propTypes = {
  statement: React.PropTypes.string,
  columns: React.PropTypes.arrayOf(React.PropTypes.string).isRequired,
  rows: React.PropTypes.arrayOf(React.PropTypes.array).isRequired,
};
