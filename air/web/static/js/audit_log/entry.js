// @flow

import React from "react";
import _ from "lodash";

type Entry = {
  user: string;
  event: string;
  inserted_at: string;
  metadata: string[][];
};

type Props = {
  entry: Entry,
  filters: string[],
};

export class AuditLogEntryView extends React.Component {
  constructor(props: Props) {
    super(props);

    this.shouldBeVisible = this.shouldBeVisible.bind(this);
    this.containsFilterText = this.containsFilterText.bind(this);
  }

  props: Props;
  shouldBeVisible: () => boolean;
  containsFilterText: (filter: string, text: string) => boolean;

  shouldBeVisible() {
    return _.every(this.props.filters, (filter) =>
      this.containsFilterText(filter, this.props.entry.user) ||
      this.containsFilterText(filter, this.props.entry.event) ||
      _.some(this.props.entry.metadata, (data) =>
        this.containsFilterText(filter, data[0]) || this.containsFilterText(filter, data[1])
      )
    );
  }

  containsFilterText(filter: string, text: string) {
    return (String(text).toLowerCase().indexOf(filter) !== -1);
  }

  render() {
    if (! this.shouldBeVisible()) {
      return null;
    }

    const metadata = this.props.entry.metadata.map((keyval, i) =>
      <li key={i}><strong>{keyval[0]}</strong>: {keyval[1]}</li>
    );
    return (
      <tr>
        <td>{this.props.entry.event}</td>
        <td>{this.props.entry.user}</td>
        <td>{this.props.entry.inserted_at}</td>
        <td><ul>{metadata}</ul></td>
      </tr>
    );
  }
}
