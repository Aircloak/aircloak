// @flow

import React from "react";

import type { PendingResult } from "./result";
import type { Authentication } from "../authentication_provider";

import CodeViewer from "../code_viewer";
import { pendingStates, later, format } from "./state";
import { cancel } from "../request";
import loader from "../../static/images/loader.gif";

type Props = {
  result: PendingResult,
  authentication: Authentication,
};

const stateItem = (state, currentState) => {
  if (later(currentState, state)) {
    return <s>{format(state)}</s>;
  } else if (state === currentState) {
    return <b>{format(state)}</b>;
  } else {
    return format(state);
  }
};

export default ({ result, authentication }: Props) => {
  return (
    <div className="card border-info">
      <div className="card-header border-info">
        <CodeViewer statement={result.statement} />
      </div>
      <div className="card-body">
        <p className="text-center spinner">
          {" "}
          <img
            src={loader}
            alt="indicating the result is being computed"
          />{" "}
        </p>
        <ul>
          {pendingStates.map((state) => (
            <li key={state}>{stateItem(state, result.query_state)}</li>
          ))}
        </ul>

        <div className="right-align">
          <button
            type="button"
            className="btn btn-small btn-warning"
            onClick={() => cancel(result.id, authentication)}
          >
            Cancel
          </button>
        </div>
      </div>
    </div>
  );
};
