// @flow

import type { Element } from "React";
import React from "react";

import CodeViewer from "../code_viewer";
import DebugExport from "./debug_export";
import ShareButton from "./share_button";
import ResultTime from "./result_time";

import type { CancelledResult } from "./result";

export default ({
  result,
  debugModeEnabled,
  onDeleteClick,
}: {
  result: CancelledResult,
  debugModeEnabled: boolean,
  onDeleteClick?: (queryId: string) => void,
}): Element<"div"> => {
  return (
    <div className="card border-warning mb-3">
      <div className="card-header border-warning bg-white">
        <ResultTime time={result.inserted_at} />
        {onDeleteClick && (
          <button
            type="button"
            className="btn btn-sm float-right"
            onClick={() => onDeleteClick(result.id)}
          >
            <i className="fas fa-times" aria-label="Delete"></i>
          </button>
        )}
        <CodeViewer statement={result.statement} />
      </div>
      <div className="card-body">
        <h4>Query cancelled</h4>
        <div className="btn-group my-2">
          <ShareButton result={result} />
          <DebugExport id={result.id} debugModeEnabled={debugModeEnabled} />
        </div>
      </div>
    </div>
  );
};
