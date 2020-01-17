// @flow

import React from "react";

import type {PendingResult} from "./result";
import type {Authentication, AuthContextType} from "../authentication_provider";
import {AuthContext} from "../authentication_provider";

import {CodeViewer} from "../code_viewer";
import {pendingStates, later, format} from "./state";
import {cancel} from "../request";

type Props = {
  result: PendingResult,
  authentication: Authentication
}

const stateItem = (state, currentState) => {
  if (later(currentState, state)) {
    return <s>{format(state)}</s>;
  } else if (state === currentState) {
    return <b>{format(state)}</b>;
  } else {
    return format(state);
  }
};

export const PendingResultView = (props: Props) => {
  const {result, authentication} = props;
  return (
    <div className="panel panel-info">
      <div className="panel-heading" />
      <div className="panel-body">
        <CodeViewer statement={result.statement} />

        <p className="text-center spinner">
          {" "}
          <img src="/images/loader.gif" role="presentation" />
          {" "}
        </p>
        <ul>
          {pendingStates.map((state, i) => <li key={i}>{stateItem(state, result.query_state)}</li>)}
        </ul>

        <div className="right-align">
          <a
            className="btn btn-small btn-warning"
            onClick={() => cancel(result.id, authentication)}
          >
            Cancel
          </a>
        </div>
      </div>
    </div>
  );
};
