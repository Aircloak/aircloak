import React from "react";

export class ResultsView extends React.Component {
  // ----------------------------------------------------------------
  // Data generating functions
  // ----------------------------------------------------------------

  renderResultRows() {
    const dateString = new Date(this.props.result.created_at * 1000).toLocaleString();

    return (
      <div className="task-results">
        <h4>Generated on {dateString}</h4>
        <table className="table table-striped table-hover">
          <thead>
            <tr>
              {this.props.result.columns.map((column) =>
                <th key={column}>{column}</th>
              )}
            </tr>
          </thead>

          <tbody>
            {this.props.result.rows.
              map((row, i) =>
                <tr key={i}>
                  {row.map((value, j) => <td key={j}>{value}</td>)}
                </tr>
              )
            }
          </tbody>
        </table>
      </div>
    );
  }

  renderEmptyResultSet() {
    return null;
  }

  renderTaskRunError(reason) {
    return (
      <div className="alert alert-danger">
        Failed to run the task on the cloak: {reason}!
      </div>
    );
  }

  // ----------------------------------------------------------------
  // React callbacks
  // ----------------------------------------------------------------

  render() {
    if (this.props.result !== undefined) {
      if (this.props.result.error) {
        return this.renderTaskRunError(this.props.result.error);
      } else {
        return this.renderResultRows();
      }
    } else {
      return this.renderEmptyResultSet();
    }
  }
}

ResultsView.propTypes = {
  result: React.PropTypes.shape({
    created_at: React.PropTypes.number.isRequired,
    exceptions: React.PropTypes.array,
    buckets: React.PropTypes.array,
    error: React.PropTypes.string,
    columns: React.PropTypes.arrayOf(React.PropTypes.string),
    rows: React.PropTypes.arrayOf(React.PropTypes.array),
  }),
};
