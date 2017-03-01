// @flow

import React from "react";

import {StateView} from "./state_view";

export type Query = {
  id: string,
  state: string,
  analyst_name: string,
  data_source_name: string,
  statement: string,
};

const maxExcerptLength = 40;

const queryExcerpt = (statement: string) => {
  if (statement.length > maxExcerptLength + 3) {
    const shortenedForm = statement.replace(/[\s\n]/g, " ").
      replace(/ +/, " ").
      slice(0, maxExcerptLength);
    return `${shortenedForm}...`;
  } else {
    return statement;
  }
};

const queryViewUrl = (props) => `/admin/queries/${props.id}`;

export const QueryView = (props: Query) =>
  <tr>
    <td>{props.data_source_name}</td>
    <td>{props.analyst_name}</td>
    <td>
      <code>{queryExcerpt(props.statement)}</code>
    </td>
    <td><StateView state={props.state} /></td>
    <td><a href={queryViewUrl(props)}>view</a></td>
  </tr>;
