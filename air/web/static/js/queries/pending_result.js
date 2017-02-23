// @flow

import React from "react";

import {CodeViewer} from "../code_viewer";
import {format} from "./state";

export const PendingResult = ({statement, query_state}: {query_state: string, statement: string}) =>
  <div className="panel panel-info">
    <div className="panel-heading" />
    <div className="panel-body">
      <CodeViewer statement={statement} />

      <p className="spinner">
        <img src="/images/loader.gif" role="presentation" /> {format(query_state)}
      </p>
    </div>
  </div>;
