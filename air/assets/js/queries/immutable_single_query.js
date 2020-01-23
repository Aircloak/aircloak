// @flow

import React from "react";

import Results from "./results";
import PropertiesView from "./properties";
import type {Authentication} from "../authentication_provider";
import type {Result} from "./result";
import type {NumberFormat} from "../number_format";

type Props = {
  result: Result,
  user: {name: string},
  insertedAt: string,
  numberFormat: NumberFormat,
  debugModeEnabled: boolean,
  authentication: Authentication,
};

export default ({result, user, insertedAt, numberFormat, debugModeEnabled, authentication}: Props) => {
  return (
    <div>
      <h3>Properties</h3>
      <PropertiesView
        user={user}
        insertedAt={insertedAt}
        dataSource={result.data_source}
        queryState={result.query_state}
      />

      <h3>Query</h3>
      <Results
        numberFormat={numberFormat}
        results={[result]}
        debugModeEnabled={debugModeEnabled}
        authentication={authentication}
      />
    </div>
  );
};
