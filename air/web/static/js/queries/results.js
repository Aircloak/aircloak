// @flow

import React from "react";

import {ResultView} from "./result";
import type {Result} from "./result";
import {PendingResult} from "./pending_result";
import {Error} from "./error";
import {Cancelled} from "./cancelled";
import {format} from "./state";

export const Results = (props: {results: Result[]}) =>
  <div>
    {props.results.map((result) => {
      switch (result.query_state) {
        case "completed":
          return <ResultView key={result.id} {...result} />;
        case "cancelled":
          return <Cancelled key={result.id} {...result} />;
        case "error":
          return <Error key={result.id} {...result} />;
        default:
          return <PendingResult key={result.id} {...result} />;
      }
    })}
  </div>;
