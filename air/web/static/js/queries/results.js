// @flow

import React from "react";

import {ResultView} from "./result";
import type {Result} from "./result";
import {PendingResult} from "./pending_result";
import {ErrorView} from "./error";

export const Results = (props: {results: Result[]}) =>
  <div>
    {props.results.map((result, i) => {
      if (result.pendingResult) {
        return <PendingResult key={i} {...result} />;
      } else if (result.columns) {
        return <ResultView key={i} {...result} />;
      } else {
        return <ErrorView key={i} {...result} />;
      }
    })}
  </div>;
