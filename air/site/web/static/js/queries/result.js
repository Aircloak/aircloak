import React from "react";

import {CodeEditor} from "../code_editor";
import {Info} from "./info";

export class Result extends React.Component {
  constructor(props) {
    super(props);

    this.state = {
      rowsToShowCount: 10,
    };

    this.renderRows = this.renderRows.bind(this);
    this.renderShowAll = this.renderShowAll.bind(this);
    this.handleClickMoreRows = this.handleClickMoreRows.bind(this);
    this.handleClickLessRows = this.handleClickLessRows.bind(this);
  }

  handleClickMoreRows() {
    this.setState({rowsToShowCount: this.props.row_count});
  }

  handleClickLessRows() {
    this.setState({rowsToShowCount: 10});
  }

  renderRows() {
    let rowsProducedCount = 0;
    return this.props.rows.reduce((rowAcc, accumulateRow, i) => {
      for (let occurenceCount = 0; occurenceCount < accumulateRow.occurrences; occurenceCount++) {
        if (rowsProducedCount >= this.state.rowsToShowCount) {
          return rowAcc;
        }
        rowsProducedCount += 1;

        const key = `${i}-${occurenceCount}`;
        const row = (<tr key={key}>
          {accumulateRow.row.map((value, j) => <td key={j}>{value}</td>)}
        </tr>);
        rowAcc.push(row);
      }
      return rowAcc;
    }, []);
  }

  renderShowAll() {
    if (this.props.row_count < 10) {
      return (
        <div className="row-count">
          {this.props.row_count} rows.
        </div>
      );
    } else if (this.props.row_count === this.state.rowsToShowCount) {
      return (
        <div className="row-count">
          {this.props.row_count} rows.&nbsp;
          <a onClick={this.handleClickLessRows}>Show fewer rows</a>
        </div>
      );
    } else {
      return (
        <div className="row-count">
          Showing 10 of {this.props.row_count} rows.&nbsp;
          <a onClick={this.handleClickMoreRows}>Show all rows</a>
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
          <Info info={this.props.info} />
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
  rows: React.PropTypes.arrayOf(React.PropTypes.shape({
    occurrences: React.PropTypes.integer,
    row: React.PropTypes.array,
  })).isRequired,
  row_count: React.PropTypes.number,
  info: Info.propTypes.info,
};
