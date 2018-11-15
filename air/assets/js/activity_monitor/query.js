// @flow

import React from "react";

import {StateView} from "./state_view";
import {cancel} from "../request";
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

export class QueryView extends React.PureComponent {
  render() {
    return (
      <tr>
        <td>{this.props.query.data_source_name}</td>
        <td>{this.props.query.cloak_name}</td>
        <td>{this.props.query.analyst_name}</td>
        <td>
          <code>{queryExcerpt(this.props.query.statement)}</code>
        </td>
        <td><StateView state={this.props.query.state} /></td>
        <td>
          <button
            className="btn btn-warning btn-xs"
            onClick={() => cancel(this.props.query.id, this.props.authentication)}
            disabled={isFinished(this.props.query.state)}
          > cancel </button>
        </td>
        <td><a href={queryViewUrl(this.props.query)}>view</a></td>
      </tr>
    );
  }
}
