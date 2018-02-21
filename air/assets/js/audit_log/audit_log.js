// @flow

import React from "react";
import Moment from "moment";

export default (props) =>
  <tr>
    <td>{props.auditLog.event}</td>
    <td>{props.auditLog.user}</td>
    <td>{formatTime(props.auditLog.time)}</td>
    <td><a href={props.auditLog.show_path}>Details</a></td>
  </tr>

const formatTime = (isoTime) => {
  const time = Moment(isoTime);
  return `${time.format("YYYY-MM-DD HH:mm:ss")} (${time.fromNow()})`;
}
