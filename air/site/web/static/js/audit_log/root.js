import React from "react";
import ReactDOM from "react-dom";

import {AuditLogEntryView} from "./entry";

class AuditLogView extends React.Component {
  constructor(props) {
    super(props);

    this.state = {
      filterText: "",
    }

    this.filterTextChange = this.filterTextChange.bind(this);
    this.filterTextToFilters = this.filterTextToFilters.bind(this);
  }

  filterTextChange(event) {
    this.setState({filterText: event.target.value});
  }

  filterTextToFilters() {
    if (this.state.filterText == "") return [];
    return this.state.filterText.toLowerCase().split(" ");
  }

  render() {
    const entries = this.props.entries.map((entry, i) => {
      return <AuditLogEntryView
        key={i}
        entry={entry}
        filters={this.filterTextToFilters()}
      />;
    });

    return (
      <div>
        <div>
          <span>Filter audit log</span>&nbsp;
          <input
            type="text"
            value={this.state.filterText}
            onChange={this.filterTextChange}
          />
          <hr />
        </div>

        <table className="table">
          <thead>
            <tr>
              <th>Event</th>
              <th>User</th>
              <th>Time</th>
              <th></th>
            </tr>
          </thead>
          <tbody>
            {entries}
          </tbody>
        </table>
      </div>
    );
  }
}

export default function renderAuditLogView(data, elem) {
  ReactDOM.render(<AuditLogView {...data} />, elem);
}

AuditLogView.propTypes = {
  entries: React.PropTypes.arrayOf(AuditLogEntryView.propTypes.entry),
};
