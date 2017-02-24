// @flow

import React from "react";

import {CodeViewer} from "../code_viewer";
import {pendingStates, later, format} from "./state";

const stateItem = (state, currentState) => {
  if (later(currentState, state)) {
    return <s>{format(state)}</s>;
  } else {
    return format(state);
  }
};

export const PendingResult = ({statement, query_state}: {query_state: string, statement: string}) =>
  <div className="panel panel-info">
    <div className="panel-heading" />
    <div className="panel-body">
      <CodeViewer statement={statement} />

      <p className="text-center spinner"> <img src="/images/loader.gif" role="presentation" /> </p>
      <ul>
        {pendingStates.map((state, i) =>
          <li key={i}>{stateItem(state, query_state)}</li>
        )}
      </ul>
    </div>
  </div>;
