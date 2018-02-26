// @flow

import React from "react";
import AuditLog from "./audit_log";

export default (props) =>
  <table className="table table-condensed">
    <thead>
      <tr>
        <th>Event</th>
        <th>User</th>
        <th>Time</th>
        <th></th>
      </tr>
    </thead>
    {(() => {
      if (props.auditLogs.length == 0) {
        return (<tr>
          <td colSpan="4">There are no audit log entries for the current set of filters.</td>
          </tr>);
      } else {
        return props.auditLogs.map((auditLog, id) => <AuditLog key={id} auditLog={auditLog} />);
      }
    })()}
  </table>
