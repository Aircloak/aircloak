import React from "react";

import {CodeEditor} from "../code_editor";

export class Result extends React.Component {
  constructor(props) {
    super(props);
  }

  render() {
    return (
      <div className="panel panel-success">
        <div className="panel-heading" />
        <div className="panel-body">
          <CodeEditor
            onRun={() => {}}
            onSave={() => {}}
            onChange={() => {}}
            statement={this.props.statement}
            readOnly
          />
          <table className="table table-striped table-hover">
            <thead>
              <tr>
                {this.props.columns.map((column) =>
                  <th key={column}>{column}</th>
                )}
              </tr>
            </thead>

            <tbody>
              {this.props.rows.
                map((row, i) =>
                  <tr key={i}>
                    {row.map((value, j) => <td key={j}>{value}</td>)}
                  </tr>
                )
              }
            </tbody>
          </table>
          <div className="row-count">
            {this.props.rows.length} rows after anonymization
          </div>
        </div>
      </div>
    );
  }

}

Result.propTypes = {
  statement: React.PropTypes.string,
  columns: React.PropTypes.arrayOf(React.PropTypes.string).isRequired,
  rows: React.PropTypes.arrayOf(React.PropTypes.array).isRequired,
};
