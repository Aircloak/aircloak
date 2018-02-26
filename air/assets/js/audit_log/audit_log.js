// @flow

import React from "react";
import Moment from "moment";
import _ from "lodash";

export default class AuditLogEntry extends React.Component {
  constructor(props) {
    super(props);

    this.state = {details: false};

    this.toggleDetails = this.toggleDetails.bind(this);
    this.renderDetails = this.renderDetails.bind(this);
    this.renderMetadata = this.renderMetadata.bind(this);
  }

  toggleDetails() {
    this.setState({details: !this.state.details});
  }

  render() {
    return (<tbody>
      <tr>
        <td>{this.props.auditLog.event}</td>
        <td>{this.props.auditLog.user}</td>
        <td>{formatTime(this.props.auditLog.time)}</td>
        <td><a href="#" onClick={this.toggleDetails}>Details</a></td>
      </tr>
      {this.renderDetails()}
    </tbody>);
  }

  renderDetails() {
    if (this.state.details) {
      return (<tr><td colSpan="4">{this.renderMetadata()}</td></tr>);
    } else {
      return null;
    }
  }

  renderMetadata() {
    return (<dl>
      {_.toPairs(this.props.auditLog.metadata).map(([key, value]) => (
        <div className="row" key={key}>
          <dt className="col-sm-3">{key}</dt>
          <dd className="col-sm-9">{value}</dd>
        </div>
      ))}
    </dl>);
  }
}

const formatTime = (isoTime) => {
  const time = Moment(isoTime);
  return `${time.format("YYYY-MM-DD HH:mm:ss")} (${time.fromNow()})`;
}
