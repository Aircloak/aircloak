// @flow

import React from "react";

import {CodeViewer} from "../code_viewer";
import {DebugExport} from "./debug_export";
import {ShareButton} from "./share_button";

import type {CancelledResult} from "./result";

export const Cancelled = (props: {result: CancelledResult, debugModeEnabled: boolean}) => {
  const {result, debugModeEnabled} = props;
  return (
    <div className="panel panel-warning">
      <div className="panel-heading" />
      <div className="panel-body">
        <CodeViewer statement={result.statement} />
        <h4>Query cancelled</h4>
        <div className="options-menu">
          <ShareButton result={result} />
          <DebugExport id={result.id} debugModeEnabled={debugModeEnabled} />
        </div>
      </div>
    </div>
  );
}
