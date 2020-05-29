// @flow
/* eslint-disable react/no-danger */

import React from "react";
import pagedown from "pagedown";

import CodeViewer from "../code_viewer";
import InfoView from "./info_view";
import DebugExport from "./debug_export";
import ShareButton from "./share_button";

import type { ErrorResult } from "./result";

const mdToHtml = (text: string) => ({
  __html: pagedown.getSanitizingConverter().makeHtml(text),
});

export default ({
  result,
  debugModeEnabled,
  onDeleteClick,
}: {
  result: ErrorResult,
  debugModeEnabled: boolean,
  onDeleteClick?: (queryId: string) => void,
}) => {
  return (
    <div className="card border-danger mb-3">
      <div className="card-header border-danger bg-white">
        <CodeViewer statement={result.statement} />
      </div>
      <div className="card-body">
        <h5 className="card-title">Query failed</h5>
        <p dangerouslySetInnerHTML={mdToHtml(result.error)} />

        <InfoView info={result.info} />

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
