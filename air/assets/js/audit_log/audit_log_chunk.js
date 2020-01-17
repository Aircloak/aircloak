// @flow

import React from "react";
import moment from "moment-timezone";
import _ from "lodash";

import AuditLogEntry from "./audit_log_entry";

import type {AuditLog} from "./audit_log_entry";

type Props = {auditLogs: Array<AuditLog>};

type State = {collapsed: boolean};

const formatTime = (isoTime: string) => {
  const time = moment.tz(isoTime, "UTC");
  return time.format("YYYY-MM-DD HH:mm:ss z");
};

export default class AuditLogChunk extends React.Component<Props, State> {
  constructor(props: Props) {
    super(props);

    this.state = {collapsed: true};

    this.numberOfEvents = this.numberOfEvents.bind(this);
    this.eventName = this.eventName.bind(this);
    this.user = this.user.bind(this);
    this.toggleCollapsed = this.toggleCollapsed.bind(this);
    this.renderEvents = this.renderEvents.bind(this);
  }

  numberOfEvents = () => {
    const {auditLogs} = this.props;
    return auditLogs.length;
  }

  eventName = () => {
    const {auditLogs} = this.props;
    return auditLogs[0].event;
  }

  user = () => {
    const {auditLogs} = this.props;
    return auditLogs[0].user;
  }

  times = () => {
    const {auditLogs} = this.props;
    return auditLogs.map<AuditLog>((auditLog) => formatTime(auditLog.time));
  }

  toggleCollapsed = (event: Event) => {
    event.preventDefault();
    const {collapsed} = this.state;
    this.setState({collapsed: !collapsed});
  }

  renderEvents = (): React$Node => {
    const {collapsed} = this.state;
    if (collapsed) {
      return null;
    } else {
      const {auditLogs} = this.props;
      return (
        <tr>
          <td colSpan="5">
            <table className="table">
              <thead>
                <tr>
                  <th>Time</th>
                  <th />
                </tr>
              </thead>
              {auditLogs.map<React$Element<typeof AuditLogEntry>>((auditLog, i) => <AuditLogEntry key={i} auditLog={auditLog} />)}
            </table>
          </td>
        </tr>
      );
    }
  }

  render = () => (
    <tbody className="panel panel-default">
      <tr className="panel-heading">
        <td>{this.eventName()}</td>
        <td>{this.numberOfEvents()}</td>
        <td>{this.user()}</td>
        <td>
          {_.min(this.times())}
          {" - "}
          {_.max(this.times())}
        </td>
        <td><a href="#" onClick={this.toggleCollapsed}>Events</a></td>
      </tr>
      {this.renderEvents()}
    </tbody>
  )
}
