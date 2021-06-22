// @flow

import type { Element } from "React";
import React from "react";

import { ResultView } from "./result";
import type { Result } from "./result";
import type { Authentication } from "../authentication_provider";
import PendingResultView from "./pending_result_view";
import ErrorView from "./error_view";
import Cancelled from "./cancelled";
import type { NumberFormat } from "../number_format";

type Props = {
  results: Result[],
  numberFormat: NumberFormat,
  debugModeEnabled: boolean,
  authentication: Authentication,
  onDeleteClick?: (id: string) => void,
  onEditNoteClick?: (result: Result) => void,
};

export default ({
  results,
  numberFormat,
  debugModeEnabled,
  authentication,
  onDeleteClick,
  onEditNoteClick,
}: Props): Element<"div"> => {
  return (
    <div>
      {results.map((result) => {
        switch (result.query_state) {
          case "completed":
            return (
              <ResultView
                key={result.id}
                result={result}
                numberFormat={numberFormat}
                debugModeEnabled={debugModeEnabled}
                onDeleteClick={onDeleteClick}
                onEditNoteClick={onEditNoteClick}
              />
            );
          case "cancelled":
            return (
              <Cancelled
                key={result.id}
                result={result}
                debugModeEnabled={debugModeEnabled}
                onDeleteClick={onDeleteClick}
                onEditNoteClick={onEditNoteClick}
              />
            );
          case "error":
            return (
              <ErrorView
                key={result.id}
                result={result}
                debugModeEnabled={debugModeEnabled}
                onDeleteClick={onDeleteClick}
                onEditNoteClick={onEditNoteClick}
              />
            );
          default:
            return (
              <PendingResultView
                key={result.id}
                authentication={authentication}
                result={result}
                onEditNoteClick={onEditNoteClick}
              />
            );
        }
      })}
    </div>
  );
};
