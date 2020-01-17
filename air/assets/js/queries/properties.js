// @flow

import React from "react";

import moment from "moment-timezone";
import {StateView} from "../activity_monitor/state_view";
import type {Result} from "./result";

export type PropertyResult = Result & {
  user: {name: string},
  inserted_at: string
}

const formatTime = (isoTime) => {
  const time = moment.tz(isoTime, "UTC");
  return `${time.format("YYYY-MM-DD HH:mm:ss z")} (${time.fromNow()})`;
};


export const PropertiesView = (props: PropertyResult) => {
  const {
    user, data_source, inserted_at, query_state
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
          <td>{data_source.name}</td>
        </tr>

        <tr>
          <td className="active col-md-2">Started on</td>
          <td>{formatTime(inserted_at)}</td>
        </tr>

        <tr>
          <td className="active col-md-2">Current state</td>
          <td>
            <StateView state={query_state} />
          </td>
        </tr>
      </tbody>
    </table>
  );
}
