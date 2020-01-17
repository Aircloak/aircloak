// @flow

import React from "react";

import {Results} from "./results";
import {PropertiesView} from "./properties";
import type {Authentication} from "../authentication_provider";
import type {Result} from "./result";
import type {NumberFormat} from "../number_format";

type Props = {
  result: Result,
  numberFormat: NumberFormat,
  debugModeEnabled: boolean,
  authentication: Authentication,
};

export default (props: Props) =>
  <div>
    <h3>Properties</h3>
    <PropertiesView {...props.result} />

    <h3>Query</h3>
    <Results
      numberFormat={props.numberFormat}
      results={[props.result]}
      debugModeEnabled={props.debugModeEnabled}
      authentication={props.authentication}
    />
  </div>;
