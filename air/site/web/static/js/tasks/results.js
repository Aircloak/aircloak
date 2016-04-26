import React from "react"

export class ResultsView extends React.Component {
  // ----------------------------------------------------------------
  // Data generating functions
  // ----------------------------------------------------------------

  renderResultRows() {
    var rows = this.props.result.buckets.map((row) => {
          var key = row.label + "-" + row.value;
          return (
            <ResultItem key={key}
              label={row.label}
              value={row.value}
              count={row.count} />
          );
        });
    var dateString = new Date(this.props.result.created_at * 1000).toString();
    return (
      <div>
        <p>
          Generated on <strong>{dateString}</strong>
        </p>
        <table className="table">
          <thead>
            <tr>
              <th>Label</th>
              <th>Value</th>
              <th>Count</th>
            </tr>
          </thead>
          <tbody>
            {rows}
          </tbody>
        </table>
      </div>
    );
  }

  renderEmptyResultSet() {
    return (
      <p>
        There are currently no results to show.
        As you results are returned from the database,
        they will be be displayed here.
      </p>
    );
  }

  renderTaskRunError() {
    return (
      <div className="alert alert-danger">
        Failed to run the task on the cloak!
      </div>
    );
  }

  // ----------------------------------------------------------------
  // React callbacks
  // ----------------------------------------------------------------

  render() {
    if (this.props.result != undefined) {
      if (this.props.result.error) {
        return this.renderTaskRunError();
      } else {
        return this.renderResultRows();
      }
    } else {
      return this.renderEmptyResultSet();
    }
  }
}

class ResultItem extends React.Component {
  render() {
    return (
      <tr>
        <td>{this.props.label}</td>
        <td>{this.props.value}</td>
        <td>{this.props.count}</td>
      </tr>
    );
  }
}
