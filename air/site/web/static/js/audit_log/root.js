import React from "react";
import ReactDOM from "react-dom";
import DatePicker from "react-datepicker";
import moment from "moment";
import $ from "jquery";

import {AuditLogEntryView} from "./entry";

class AuditLogView extends React.Component {
  constructor(props) {
    super(props);

    this.state = {
      filterText: "",
      startDate: moment().subtract(1, "week"),
      endDate: moment(),
      loadingData: false,
      entries: props.entries,
    };

    this.filterTextChange = this.filterTextChange.bind(this);
    this.filterTextToFilters = this.filterTextToFilters.bind(this);
    this.handleDateStartChange = this.handleDateStartChange.bind(this);
    this.handleDateEndChange = this.handleDateEndChange.bind(this);
    this.loadData = this.loadData.bind(this);
  }

  filterTextChange(event) {
    const filterText = event.target.value;
    this.setState({filterText});
  }

  filterTextToFilters() {
    if (this.state.filterText === "") return [];
    return this.state.filterText.toLowerCase().split(" ");
  }

  handleDateStartChange(startDate) {
    this.setState({startDate});
    this.loadData(startDate, this.state.endDate);
  }

  handleDateEndChange(endDate) {
    this.setState({endDate});
    this.loadData(this.state.startDate, endDate);
  }

  loadData(startDate, endDate) {
    this.setState({loadingData: true});

    const data = {
      from: startDate.format("YYYY-MM-DD"),
      to: endDate.format("YYYY-MM-DD"),
    };

    $.ajax("/audit_log/load_entries", {
      method: "GET",
      data,
      success: (response) => {
        this.setState({entries: JSON.parse(response)});
        this.setState({loadingData: false});
      },
    });
  }

  render() {
    const entries = this.state.entries.map((entry, i) =>
      <AuditLogEntryView
        key={i}
        entry={entry}
        filters={this.filterTextToFilters()}
      />
    );

    let loading = null;
    if (this.state.loadingData) {
      loading = (
        <span className="entries-loader">
          <img role="presentation" src="/images/loader.gif" />
          loading log entries
        </span>);
    }

    return (
      <div>
        <h2>Audit log {loading}</h2>
        <hr />

        <div className="controls">
          <div className="filter">
            <span>Filter audit log</span>&nbsp;
            <input
              type="text"
              value={this.state.filterText}
              onChange={this.filterTextChange}
            />
          </div>

          <div className="time-interval">
            <span>Time interval</span>&nbsp;
            <DatePicker
              dateFormat="YYYY/MM/DD"
              showYearDropdown
              selected={this.state.startDate}
              startDate={this.state.startDate}
              endDate={this.state.endDate}
              onChange={this.handleDateStartChange}
            />
            <DatePicker
              dateFormat="YYYY/MM/DD"
              showYearDropdown
              todayButton="Today"
              selected={this.state.endDate}
              startDate={this.state.startDate}
              endDate={this.state.endDate}
              onChange={this.handleDateEndChange}
            />
          </div>
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
