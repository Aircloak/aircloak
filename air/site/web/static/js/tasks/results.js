import React from "react";

export class ResultsView extends React.Component {
  // ----------------------------------------------------------------
  // Data generating functions
  // ----------------------------------------------------------------

  renderResultRows() {
    const dateString = new Date(this.props.result.created_at * 1000).toLocaleString();
    return (
      <div>
        <p>
          Generated on <strong>{dateString}</strong>
        </p>
        <Buckets buckets={this.props.result.buckets} />
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

const Buckets = (props) => {
  if (props.buckets.length === 0) {
    return (<p>The task returned no results.</p>);
  } else {
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
          {props.buckets.map((item) =>
            <tr key={`${item.label}_${item.value}`}>
              <td>{item.label}</td>
              <td>{item.value}</td>
              <td>{item.count}</td>
            </tr>
          )}
        </tbody>
      </table>
    );
  }
};

Buckets.propTypes = {
  buckets: React.PropTypes.arrayOf(React.PropTypes.shape({
    label: React.PropTypes.string,
    value: React.PropTypes.string,
    count: React.PropTypes.number,
  })),
};
