// @flow

import React from "react";
import moment from "moment-timezone";
import _ from "lodash";

export type AuditLog = {
  event: string,
  user: string,
  time: string,
  metadata: {},
};

type Props = {auditLog: AuditLog};

type State = {details: boolean};

const formatTime = (isoTime) => {
  const time = moment.tz(isoTime, "UTC");
  return `${time.format("YYYY-MM-DD HH:mm:ss z")} (${time.fromNow()})`;
};

export default class AuditLogEntry extends React.Component {
  constructor(props: Props) {
    super(props);

    this.state = {details: false};

    this.toggleDetails = this.toggleDetails.bind(this);
    this.renderDetails = this.renderDetails.bind(this);
    this.renderMetadata = this.renderMetadata.bind(this);
  }

  state: State;
  toggleDetails: () => void;
  renderDetails: () => void;
  renderMetadata: () => void;

  toggleDetails(event: Event) {
    event.preventDefault();
    this.setState({details: !this.state.details});
  }

  render() {
    return (<tbody>
      <tr>
        <td>{formatTime(this.props.auditLog.time)}</td>
        <td><a href="#" onClick={this.toggleDetails}>Details</a></td>
      </tr>
      {this.renderDetails()}
    </tbody>);
  }

  renderDetails() {
    if (this.state.details) {
      return (<tr><td colSpan="2">{this.renderMetadata()}</td></tr>);
    } else {
      return null;
    }
  }

  stringify(value) {
    if (value !== undefined && value !== null) {
      return value.toString();
    } else {
      return "not provided";
    }
  }

  renderMetadata() {
    return (<dl>
      {_.toPairs(this.props.auditLog.metadata).map(([key, value]) => (
        <div className="row" key={key}>
          <dt className="col-sm-3">{this.stringify(key)}</dt>
          <dd className="col-sm-9">{this.stringify(value)}</dd>
        </div>
      ))}
    </dl>);
  }
}
