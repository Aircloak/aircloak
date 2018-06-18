// @flow

import React from "react";
import moment from "moment";
import _ from "lodash";

import AuditLogEntry from "./audit_log_entry";

import type {AuditLog} from "./audit_log_entry";

type Props = {auditLogs: Array<AuditLog>};

type State = {collapsed: boolean};

const formatTime = (isoTime) => {
  const time = moment(isoTime);
  return time.format("YYYY-MM-DD HH:mm:ss");
};

export default class AuditLogChunk extends React.Component {
  constructor(props: Props) {
    super(props);

    this.state = {collapsed: true};

    this.numberOfEvents = this.numberOfEvents.bind(this);
    this.eventName = this.eventName.bind(this);
    this.user = this.user.bind(this);
    this.toggleCollapsed = this.toggleCollapsed.bind(this);
    this.renderEvents = this.renderEvents.bind(this);
  }

  state: State;
  numberOfEvents: () => number;
  eventName: () => string;
  user: () => string;
  toggleCollapsed: (event: Event) => void;
  renderEvents: () => Node;

  numberOfEvents() {
    return this.props.auditLogs.length;
  }

  eventName() {
    return this.props.auditLogs[0].event;
  }

  user() {
    return this.props.auditLogs[0].user;
  }

  times() {
    return this.props.auditLogs.map((auditLog) => formatTime(auditLog.time));
  }

  toggleCollapsed(event: Event) {
    event.preventDefault();
    this.setState({collapsed: !this.state.collapsed});
  }

  renderEvents() {
    if (this.state.collapsed) {
      return null;
    } else {
      return (<tr><td colSpan="5">
        <table className="table">
          <thead>
            <tr>
              <th>Time</th>
              <th></th>
            </tr>
          </thead>
          {this.props.auditLogs.map((auditLog, i) =>
            <AuditLogEntry key={i} auditLog={auditLog} />)}
        </table>
      </td></tr>);
    }
  }

  render() {
    return (<tbody className="panel panel-default">
      <tr className="panel-heading">
        <td>{this.eventName()}</td>
        <td>{this.numberOfEvents()}</td>
        <td>{this.user()}</td>
        <td>{_.min(this.times())} - {_.max(this.times())}</td>
        <td><a href="#" onClick={this.toggleCollapsed}>Events</a></td>
      </tr>
      {this.renderEvents()}
    </tbody>);
  }
}
