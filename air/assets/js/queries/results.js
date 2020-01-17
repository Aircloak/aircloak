// @flow

import React from "react";

import {ResultView} from "./result";
import type {Result} from "./result";
import type {Authentication} from "../authentication_provider";
import {PendingResultView} from "./pending_result";
import {Error} from "./error";
import {Cancelled} from "./cancelled";
import type {NumberFormat} from "../number_format";

type Props = {
  results: Result[],
  numberFormat: NumberFormat,
  debugModeEnabled: boolean,
  authentication: Authentication
};

export const Results = (props: Props) => {
  const {
    results, numberFormat, debugModeEnabled, authentication,
  } = props;
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
              />
            );
          case "cancelled":
            return (
              <Cancelled
                key={result.id}
                result={result}
                debugModeEnabled={debugModeEnabled}
              />
            );
          case "error":
            return (
              <Error
                key={result.id}
                result={result}
                debugModeEnabled={debugModeEnabled}
              />
            );
          default:
            return <PendingResultView key={result.id} authentication={authentication} result={result} />;
        }
      })}
    </div>
  );
};
