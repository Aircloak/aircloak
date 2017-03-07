// @flow

import React from "react";

import type {Result} from "./result";
import {CodeViewer} from "../code_viewer";
import {pendingStates, later, format} from "./state";
import {cancel} from "../request";

type Props = {
  result: Result,
  CSRFToken: string,
}

const stateItem = (state, currentState) => {
  if (later(currentState, state)) {
    return <s>{format(state)}</s>;
  } else {
    return format(state);
  }
};

export const PendingResult = (props: Props) =>
  <div className="panel panel-info">
    <div className="panel-heading" />
    <div className="panel-body">
      <CodeViewer statement={props.result.statement} />

      <p className="text-center spinner"> <img src="/images/loader.gif" role="presentation" /> </p>
      <ul>
        {pendingStates.map((state, i) =>
          <li key={i}>{stateItem(state, props.result.query_state)}</li>
        )}
      </ul>

      <div className="right-align">
        <a
          className="btn btn-small btn-warning"
          onClick={() => cancel(props.result.id, props.CSRFToken)}
        >Cancel</a>
      </div>
    </div>
  </div>;
