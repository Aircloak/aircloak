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
  hideCancelButton?: boolean,
  showDataSource?: boolean,
  onDeleteClick?: (id: string) => void,
  updateNote?: (id: string, note: string | null) => void,
};

export default ({
  results,
  numberFormat,
  debugModeEnabled,
  authentication,
  hideCancelButton,
  showDataSource,
  onDeleteClick,
  updateNote,
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
                showDataSource={showDataSource}
                onDeleteClick={onDeleteClick}
                updateNote={updateNote}
              />
            );
          case "cancelled":
            return (
              <Cancelled
                key={result.id}
                result={result}
                debugModeEnabled={debugModeEnabled}
                showDataSource={showDataSource}
                onDeleteClick={onDeleteClick}
                updateNote={updateNote}
              />
            );
          case "error":
            return (
              <ErrorView
                key={result.id}
                result={result}
                debugModeEnabled={debugModeEnabled}
                showDataSource={showDataSource}
                onDeleteClick={onDeleteClick}
                updateNote={updateNote}
              />
            );
          default:
            return (
              <PendingResultView
                key={result.id}
                authentication={authentication}
                result={result}
                showDataSource={showDataSource}
                updateNote={updateNote}
                hideCancelButton={hideCancelButton}
              />
            );
        }
      })}
    </div>
  );
};
