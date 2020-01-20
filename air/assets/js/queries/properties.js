// @flow

import React from "react";

import moment from "moment-timezone";
import StateView from "../activity_monitor/state_view";

type Props = {
  user: {name: string},
  queryState: string,
  insertedAt: string,
  dataSource: {name: string},
}

const formatTime = (isoTime) => {
  const time = moment.tz(isoTime, "UTC");
  return `${time.format("YYYY-MM-DD HH:mm:ss z")} (${time.fromNow()})`;
};

export default (props: Props) => {
  const {
    user, dataSource, insertedAt, queryState,
  } = props;
  return (
    <table className="table table-condensed">
      <tbody>
        <tr>
          <td className="active col-md-2">User</td>
          <td>{user.name}</td>
        </tr>

        <tr>
          <td className="active col-md-2">Data source</td>
          <td>{dataSource.name}</td>
        </tr>

        <tr>
          <td className="active col-md-2">Started on</td>
          <td>{formatTime(insertedAt)}</td>
        </tr>

        <tr>
          <td className="active col-md-2">Current state</td>
          <td>
            <StateView state={queryState} />
          </td>
        </tr>
      </tbody>
    </table>
  );
};
