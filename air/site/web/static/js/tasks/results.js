import React from "react"

export class ResultsView extends React.Component {
  // ----------------------------------------------------------------
  // Data generating functions
  // ----------------------------------------------------------------

  renderResultRows() {
    var dateString = new Date(this.props.result.created_at * 1000).toString();
    return (
      <div>
        <p>
          Generated on <strong>{dateString}</strong>
        </p>
        <Errors errors={this.props.result.exceptions} />
        <Buckets buckets={this.props.result.buckets} />
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
    if (this.props.result != undefined) {
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

class Buckets extends React.Component {
  render() {
    if (this.props.buckets.length == 0)
      return (<p>The task returned no results.</p>)

    return (
          <table className="table">
            <thead>
              <tr>
                <th>Label</th>
                <th>Value</th>
                <th>Count</th>
              </tr>
            </thead>
            <tbody>
              {this.props.buckets.map((item) =>
                    <tr key={`${item.label}_${item.value}`}>
                      <td>{item.label}</td>
                      <td>{item.value}</td>
                      <td>{item.count}</td>
                    </tr>
                  )}
            </tbody>
          </table>
        )
  }
}

class Errors extends React.Component {
  render() {
    if (this.props.errors.length == 0)
      return null;

    return (
          <div className="alert alert-danger">
            <p>Following errors were reported:</p>
            <ul>
              {this.props.errors.map((item) =>
                    <li key={item.error}>{item.error} ({item.count})</li>
                  )}
            </ul>
          </div>
        )
  }
}
