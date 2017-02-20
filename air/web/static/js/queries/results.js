// @flow

import React from "react";

import {ResultView} from "./result";
import type {Result} from "./result";
import {PendingResult} from "./pending_result";
import {Error} from "./error";

export const Results = (props: {results: Result[]}) =>
  <div>
    {props.results.map((result) => {
      if (result.pendingResult) {
        return <PendingResult key={result.id} {...result} />;
      } else if (result.columns) {
        return <ResultView key={result.id} {...result} />;
      } else {
        return <Error key={result.id} {...result} />;
      }
    })}
  </div>;
