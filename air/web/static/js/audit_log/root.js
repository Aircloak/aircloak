// @flow

import React from "react";
import ReactDOM from "react-dom";
import $ from "jquery";

import {AuditLogEntryView} from "./entry";
import type {Entry} from "./entry";
import {DateFilter} from "./date_filter";
import {FilterControl} from "./filter_control";

type Props = {entries: Entry[]}

class AuditLogView extends React.Component {
  constructor(props) {
    super(props);

    this.state = {
      filterText: "",
      loadingData: false,
      entries: props.entries,
    };

    this.filterTextChange = this.filterTextChange.bind(this);
    this.filterTextToFilters = this.filterTextToFilters.bind(this);
    this.loadData = this.loadData.bind(this);
  }

  state: {filterText: string, loadingData: boolean, entries: Entry[]}
  props: Props;
  filterTextChange: () => void;
  filterTextToFilters: () => void;
  loadData: () => void;

  filterTextChange(event) {
    const filterText = event.target.value;
    this.setState({filterText});
  }

  filterTextToFilters() {
    if (this.state.filterText === "") return [];
    return this.state.filterText.toLowerCase().split(" ");
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

        <div className="controls">
          <FilterControl value={this.state.filterText} onChange={this.filterTextChange} className="filter" />
          <DateFilter handleDateChange={this.loadData} className="time-interval" />
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

export default function renderAuditLogView(data: Props, elem: Node) {
  ReactDOM.render(<AuditLogView {...data} />, elem);
}
