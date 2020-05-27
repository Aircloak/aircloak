// @flow

import React from "react";

import CodeViewer from "../code_viewer";
import DebugExport from "./debug_export";
import ShareButton from "./share_button";
import DeleteButton from "./delete_button";

import type { CancelledResult } from "./result";

export default ({
  result,
  debugModeEnabled,
  onDeleteClick,
}: {
  result: CancelledResult,
  debugModeEnabled: boolean,
  onDeleteClick?: (queryId: string) => void,
}) => {
  return (
    <div className="card border-warning mb-3">
      <div className="card-header bg-warning" />
      <div className="card-body">
        <CodeViewer statement={result.statement} />
        <h4>Query cancelled</h4>
        <div className="options-menu">
          <ShareButton result={result} />
          <DebugExport id={result.id} debugModeEnabled={debugModeEnabled} />
          <DeleteButton id={result.id} onClick={onDeleteClick} />
        </div>
      </div>
    </div>
  );
};
