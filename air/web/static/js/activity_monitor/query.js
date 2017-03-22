// @flow

import React from "react";

import {StateView} from "./state_view";
import {cancel} from "../request";
import type {Authentication} from "../request";
import {isFinished} from "../queries/state";

export type Query = {
  id: string,
  state: string,
  analyst_name: string,
  data_source_name: string,
  cloak_name: string,
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

export const QueryView = ({query}: {query: Query}, context: {authentication: Authentication}) =>
  <tr>
    <td>{query.data_source_name}</td>
    <td>{query.cloak_name}</td>
    <td>{query.analyst_name}</td>
    <td>
      <code>{queryExcerpt(query.statement)}</code>
    </td>
    <td><StateView state={query.state} /></td>
    <td>
      <button
        className="btn btn-warning btn-xs"
        onClick={() => cancel(query.id, context.authentication)}
        disabled={isFinished(query.state)}
      > cancel </button>
    </td>
    <td><a href={queryViewUrl(query)}>view</a></td>
  </tr>;

QueryView.contextTypes = {
  authentication: React.PropTypes.object.isRequired,
};
