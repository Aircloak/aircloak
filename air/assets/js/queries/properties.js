// @flow

import type { Element } from "React";
import React from "react";

import moment from "moment-timezone";
import StateView from "../active_queries/state_view";

type Props = {
  user: { name: string },
  queryState: string,
  insertedAt: string,
  dataSource: { name: string },
};

const formatTime = (isoTime) => {
  const time = moment.tz(isoTime, "UTC");
  return `${time.format("YYYY-MM-DD HH:mm:ss z")} (${time.fromNow()})`;
};

export default ({
  user,
  dataSource,
  insertedAt,
  queryState,
}: Props): Element<"dl"> => {
  return (
    <dl className="row">
      <dt className="col-sm-3 col-md-2">User</dt>
      <dd className="col-sm-9 col-md-10">{user.name}</dd>

      <dt className="col-sm-3 col-md-2">Data source</dt>
      <dd className="col-sm-9 col-md-10">{dataSource.name}</dd>

      <dt className="col-sm-3 col-md-2">Started on</dt>
      <dd className="col-sm-9 col-md-10">{formatTime(insertedAt)}</dd>

      <dt className="col-sm-3 col-md-2">Current state</dt>
      <dd className="col-sm-9 col-md-10">
        <StateView queryState={queryState} />
      </dd>
    </dl>
  );
};
