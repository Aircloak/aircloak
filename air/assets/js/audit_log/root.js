// @flow

import React from "react";
import AuditLogEntry from "./audit_log";
import _ from "lodash";

import type {AuditLog} from "./audit_log";

type Props = {auditLogs: [AuditLog]};

export default (props: Props) =>
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
      if (_.isEmpty(props.auditLogs)) {
        return (<tbody><tr>
          <td colSpan="4">There are no audit log entries for the current set of filters.</td>
        </tr></tbody>);
      } else {
        return props.auditLogs.map((auditLog, id) => <AuditLogEntry key={id} auditLog={auditLog} />);
      }
    })()}
  </table>;
