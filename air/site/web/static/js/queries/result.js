import React from "react";

import {CodeEditor} from "../code_editor";

export class Result extends React.Component {
  constructor(props) {
    super(props);

    this.renderRows = this.renderRows.bind(this);
    this.renderShowAll = this.renderShowAll.bind(this);
  }


  renderRows() {
    const tableRows = this.props.rows.map((row, i) =>
      <tr key={i}>
        {row.map((value, j) => <td key={j}>{value}</td>)}
      </tr>
    );
    return tableRows;
  }

  renderShowAll() {
    if (this.props.isLoading) {
      return (
        <div className="row-count">
          <img role="presentation" src="/images/loader.gif" /> loading rows
        </div>
      );
    }
    if (this.props.row_count < 10) {
      return (
        <div className="row-count">
          {this.props.row_count} rows.
        </div>
      );
    } else if (this.props.row_count === this.props.rows.length) {
      return (
        <div className="row-count">
          {this.props.row_count} rows.&nbsp;
          <a onClick={() => this.props.handleLessRows(this.props)}>Show fewer rows</a>
        </div>
      );
    } else {
      return (
        <div className="row-count">
          Showing 10 of {this.props.row_count} rows.&nbsp;
          <a onClick={() => this.props.handleLoadRows(this.props)}>Show all rows</a>
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
  row_count: React.PropTypes.number,
  isLoading: React.PropTypes.bool,
  handleLessRows: React.PropTypes.func,
  handleLoadRows: React.PropTypes.func,
};
