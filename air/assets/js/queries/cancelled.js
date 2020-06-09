// @flow

import React from "react";

import CodeViewer from "../code_viewer";
import DebugExport from "./debug_export";
import ShareButton from "./share_button";

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
      <div className="card-header border-danger bg-warning">
        <CodeViewer statement={result.statement} />
      </div>
      <div className="card-body">
        <h4>Query cancelled</h4>
        <div className="options-menu">
          <ShareButton result={result} />
          <DebugExport id={result.id} debugModeEnabled={debugModeEnabled} />
          {onDeleteClick && (
            <button
              type="button"
              className="btn btn-danger btn-sm"
              onClick={() => onDeleteClick(result.id)}
            >
              Delete
            </button>
          )}
        </div>
      </div>
    </div>
  );
};
