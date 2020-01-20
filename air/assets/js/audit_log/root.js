// @flow

import React from "react";
import _ from "lodash";
import AuditLogChunk from "./audit_log_chunk";
import chunkBy from "./chunkBy";

import type {AuditLog} from "./audit_log_entry";

type Props = {auditLogs: Array<AuditLog>};

const auditLogKey = (auditLog) => [auditLog.event, auditLog.user];

export default (props: Props) => (
  <table className="table table-condensed">
    <thead>
      <tr>
        <th>Event</th>
        <th>Occurences</th>
        <th>User</th>
        <th>Time</th>
        <th>{" "}</th>
      </tr>
    </thead>
    {(() => {
      if (_.isEmpty(props.auditLogs)) {
        return (
          <tbody>
            <tr>
              <td colSpan="5">There are no audit log entries for the current set of filters.</td>
            </tr>
          </tbody>
        );
      } else {
        return chunkBy(props.auditLogs, auditLogKey).map(
          // eslint-disable-next-line react/no-array-index-key
          (auditLogs, id) => <AuditLogChunk key={id} auditLogs={auditLogs} />,
        );
      }
    })()}
  </table>
);
