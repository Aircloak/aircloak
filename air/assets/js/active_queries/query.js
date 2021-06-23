// @flow

import type { Element } from "React";
import type { AuthContextType } from "../authentication_provider";
import React from "react";

import StateView from "./state_view";
import { cancel } from "../request";
import { isFinished } from "../queries/state";
import { AuthContext } from "../authentication_provider";

export type Query = {
  id: string,
  state: string,
  analyst_name: string,
  data_source_name: string,
  cloak_name: string,
  statement: string,
};

type Props = {
  query: Query,
};

const maxExcerptLength = 40;

const queryExcerpt = (statement: string) => {
  if (statement.length > maxExcerptLength + 3) {
    const shortenedForm = statement
      .replace(/[\s\n]/g, " ")
      .replace(/ +/, " ")
      .slice(0, maxExcerptLength);
    return `${shortenedForm}...`;
  } else {
    return statement;
  }
};

const queryViewUrl = (query: Query) => `/admin/queries/${query.id}`;

export class QueryView extends React.Component<Props> {
  // eslint-disable-next-line react/static-property-placement
  static contextType: React$Context<AuthContextType> = AuthContext;

  shouldComponentUpdate(nextProps: Props): boolean {
    const { query } = this.props;
    return nextProps.query.state !== query.state;
  }

  render(): Element<"tr"> {
    const { query } = this.props;
    const { authentication } = this.context;
    return (
      <tr>
        <td>{query.data_source_name}</td>
        <td>{query.cloak_name}</td>
        <td>{query.analyst_name}</td>
        <td>
          <code>{queryExcerpt(query.statement)}</code>
        </td>
        <td>
          <StateView queryState={query.state} />
        </td>
        <td>
          <button
            type="button"
            className="btn btn-warning btn-sm"
            onClick={() => cancel(query.id, authentication)}
            disabled={isFinished(query.state)}
          >
            cancel
          </button>
        </td>
        <td>
          <a href={queryViewUrl(query)}>view</a>
        </td>
      </tr>
    );
  }
}
