import React from "react";

export class ResultsView extends React.Component {
  // ----------------------------------------------------------------
  // Data generating functions
  // ----------------------------------------------------------------

  renderResultRows() {
    const dateString = new Date(this.props.result.created_at * 1000).toLocaleString();
    return (
      <div>
        <h3>Generated on {dateString}</h3>
        <table className="table table-striped table-hover">
          <thead>
            <tr>
              {this.props.result.columns.map((column) =>
                <th key={column}>{column}</th>
              )}
            </tr>
          </thead>

          <tbody>
            {this.props.result.rows.map((row, i) =>
              <tr key={i}>
                {row.map((value, i) => <td key={i}>{value}</td>)}
              </tr>
            )}
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
  }),
};
