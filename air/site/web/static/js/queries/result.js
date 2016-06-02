import React from "react";

import {CodeEditor} from "../code_editor";

export class Result extends React.Component {
  constructor(props) {
    super(props);

    if (props.rows.length > 10) {
      this.state = {showAll: false};
    } else {
      this.state = {showAll: true};
    }

    this.renderRows = this.renderRows.bind(this);
    this.renderShowAll = this.renderShowAll.bind(this);
    this.clickShowAll = this.clickShowAll.bind(this);
  }

  clickShowAll() {
    this.setState({showAll: true});
  }

  renderRows() {
    let rows = this.props.rows;
    if (! this.state.showAll) {
      rows = rows.slice(0, 10);
    }
    const tableRows = rows.map((row, i) =>
      <tr key={i}>
        {row.map((value, j) => <td key={j}>{value}</td>)}
      </tr>
    );
    return tableRows;
  }

  renderShowAll() {
    if (this.state.showAll) {
      return (
        <div className="row-count">
          {this.props.rows.length} rows.
        </div>
      );
    } else {
      return (
        <div className="row-count">
          Showing 10 of {this.props.rows.length} rows. <a onClick={this.clickShowAll}>Show all rows</a>
        </div>
      );
    }
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
              {this.renderRows()}
            </tbody>
          </table>
          {this.renderShowAll()}
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
