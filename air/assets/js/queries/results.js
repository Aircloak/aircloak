// @flow

import React from "react";

import {ResultView} from "./result";
import type {Result} from "./result";
import {PendingResult} from "./pending_result";
import {Error} from "./error";
import {Cancelled} from "./cancelled";
import type {NumberFormat} from "../number_format";

type Props = {
  results: Result[],
  numberFormat: NumberFormat,
  debugModeEnabled: boolean,
  shareButton: boolean
};

export const Results = (props: Props) =>
  <div>
    {props.results.map((result) => {
      switch (result.query_state) {
        case "completed":
          return (<ResultView
            key={result.id}
            result={result}
            numberFormat={props.numberFormat}
            debugModeEnabled={props.debugModeEnabled}
            shareButton={props.shareButton}
          />);
        case "cancelled":
          return (<Cancelled
            key={result.id}
            result={result}
            debugModeEnabled={props.debugModeEnabled}
            shareButton={props.shareButton}
          />);
        case "error":
          return (<Error
            key={result.id}
            result={result}
            debugModeEnabled={props.debugModeEnabled}
            shareButton={props.shareButton}
          />);
        default:
          return <PendingResult key={result.id} result={result} />;
      }
    })}
  </div>;
