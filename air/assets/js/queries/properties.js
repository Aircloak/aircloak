// @flow

import React from "react";

import {StateView} from "../activity_monitor/state_view";
import type {Result} from "./result";
import moment from "moment-timezone";

const formatTime = (isoTime) => {
  const time = moment.tz(isoTime, "UTC");
  return `${time.format("YYYY-MM-DD HH:mm:ss z")} (${time.fromNow()})`;
};


export const PropertiesView = (props: Result) =>
  <table className="table table-condensed">
    <tbody>
      <tr>
        <td className="active col-md-2">User</td>
        <td>{props.user.name}</td>
      </tr>

      <tr>
        <td className="active col-md-2">Data source</td>
        <td>{props.data_source.name}</td>
      </tr>

      <tr>
        <td className="active col-md-2">Started on</td>
        <td>{formatTime(props.inserted_at)}</td>
      </tr>

      <tr>
        <td className="active col-md-2">Current state</td>
        <td>
          <StateView state={props.query_state} />
        </td>
      </tr>
    </tbody>
  </table>;
