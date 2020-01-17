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

export default class AuditLogEntry extends React.Component<Props, State> {
  constructor(props: Props) {
    super(props);

    this.state = {details: false};

    this.toggleDetails = this.toggleDetails.bind(this);
    this.renderDetails = this.renderDetails.bind(this);
    this.renderMetadata = this.renderMetadata.bind(this);
  }

  toggleDetails = (event: Event) => {
    event.preventDefault();
    const {details} = this.state;
    this.setState({details: !details});
  }

  render = () => {
    const {auditLog} = this.props;
    return (
      <tbody>
        <tr>
          <td>{formatTime(auditLog.time)}</td>
          <td><a href="#" onClick={this.toggleDetails}>Details</a></td>
        </tr>
        {this.renderDetails()}
      </tbody>
    );
  }

  renderDetails = () => {
    const {details} = this.state;
    if (details) {
      return (<tr><td colSpan="2">{this.renderMetadata()}</td></tr>);
    } else {
      return null;
    }
  }

  renderMetadata = () => {
    const {auditLog} = this.props;
    return (
      <dl>
        {_.toPairs(auditLog.metadata).map(([key, value]) => (
          <div className="row" key={key}>
            <dt className="col-sm-3">{_.toString(key)}</dt>
            <dd className="col-sm-9">{_.toString(value)}</dd>
          </div>
        ))}
      </dl>
    );
  }
}
