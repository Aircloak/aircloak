// @flow

import React from "react";

import {StateView} from "./state_view";

export type Query = {
  id: string,
  state: string,
  analyst_name: string,
  data_source_name: string,
};

export const QueryView = (props: Query) =>
  <tr>
    <td>{props.data_source_name}</td>
    <td>{props.analyst_name}</td>
    <td><StateView state={props.state} /></td>
  </tr>;
