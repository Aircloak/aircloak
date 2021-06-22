// @flow

import type { Element } from "React";
import React from "react";

import type { PendingResult } from "./result";
import type { Authentication } from "../authentication_provider";

import CodeViewer from "../code_viewer";
import ResultTime from "./result_time";
import QueryNote from "./query_note";
import { pendingStates, later, format } from "./state";
import { cancel } from "../request";
import loader from "../../static/images/loader.gif";

type Props = {
  result: PendingResult,
  authentication: Authentication,
  onEditNoteClick?: (result: PendingResult) => void,
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

export default ({
  result,
  authentication,
  onEditNoteClick,
}: Props): Element<"div"> => {
  return (
    <div className="card border-info mb-3">
      <div className="card-header border-info bg-white">
        <ResultTime time={result.inserted_at} />
        {onEditNoteClick && (
          <button
            type="button"
            className="btn btn-sm float-right"
            onClick={() => onEditNoteClick(result)}
          >
            <i className="far fa-comment-alt" aria-label="Set note"></i>
          </button>
        )}
        <CodeViewer statement={result.statement} />
      </div>
      <div className="card-body">
        <QueryNote note={result.note} />
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
