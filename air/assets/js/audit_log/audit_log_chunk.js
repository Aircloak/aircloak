// @flow

import React from "react";
import moment from "moment";
import _ from "lodash";

import AuditLogEntry from "./audit_log";

const formatTime = (isoTime) => {
  const time = moment(isoTime);
  return time.format("YYYY-MM-DD HH:mm:ss");
};


export default class AuditLogChunk extends React.Component {
  constructor(props) {
    super(props);

    this.state = {collapsed: true};

    this.numberOfEvents = this.numberOfEvents.bind(this);
    this.eventName = this.eventName.bind(this);
    this.user = this.user.bind(this);
  }

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

  render() {
    if (this.numberOfEvents() === 1) {
      return (<AuditLogEntry auditLog={this.props.auditLogs[0]} />);
    } else {
      return (<tbody><tr>
        <td>{this.eventName()} ({this.numberOfEvents()} times)</td>
        <td>{this.user()}</td>
        <td>{_.min(this.times())} - {_.max(this.times())}</td>
        <td></td>
      </tr></tbody>);
    }
  }
};
