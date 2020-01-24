// @flow

import React from "react";
import moment from "moment-timezone";
import _ from "lodash";

import AuditLogEntry from "./audit_log_entry";

import type { AuditLog } from "./audit_log_entry";

type Props = { auditLogs: Array<AuditLog> };

type State = { collapsed: boolean };

const formatTime = (isoTime: string) => {
  const time = moment.tz(isoTime, "UTC");
  return time.format("YYYY-MM-DD HH:mm:ss z");
};

export default class AuditLogChunk extends React.Component<Props, State> {
  constructor(props: Props) {
    super(props);

    this.state = { collapsed: true };

    this.toggleCollapsed = this.toggleCollapsed.bind(this);
    this.renderEvents = this.renderEvents.bind(this);
  }

  times = () =>
    this.props.auditLogs.map<AuditLog>(auditLog => formatTime(auditLog.time));

  toggleCollapsed = (event: Event) => {
    event.preventDefault();
    this.setState({ collapsed: !this.state.collapsed });
  };

  renderEvents = (): React$Node => {
    if (this.state.collapsed) {
      return null;
    } else {
      return (
        <tr>
          <td colSpan="5">
            <table className="table">
              <thead>
                <tr>
                  <th>Time</th>
                  <th> </th>
                </tr>
              </thead>
              {this.props.auditLogs.map<React$Element<typeof AuditLogEntry>>(
                (auditLog, i) => (
                  // eslint-disable-next-line react/no-array-index-key
                  <AuditLogEntry key={i} auditLog={auditLog} />
                )
              )}
            </table>
          </td>
        </tr>
      );
    }
  };

  render = () => (
    <tbody className="panel panel-default">
      <tr className="panel-heading">
        <td>{this.props.auditLogs[0].event}</td>
        <td>{this.props.auditLogs.length}</td>
        <td>{this.props.auditLogs[0].user}</td>
        <td>
          {_.min(this.times())}
          {" - "}
          {_.max(this.times())}
        </td>
        <td>
          <button type="button" onClick={this.toggleCollapsed}>
            Events
          </button>
        </td>
      </tr>
      {this.renderEvents()}
    </tbody>
  );
}
