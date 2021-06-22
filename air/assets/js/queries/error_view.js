// @flow
import type { Element } from "React"; /* eslint-disable react/no-danger */

import React from "react";
import pagedown from "pagedown";

import CodeViewer from "../code_viewer";
import InfoView from "./info_view";
import DebugExport from "./debug_export";
import ShareButton from "./share_button";
import ResultTime from "./result_time";
import QueryNote from "./query_note";

import type { ErrorResult } from "./result";

const mdToHtml = (text: string) => ({
  __html: pagedown.getSanitizingConverter().makeHtml(text),
});

export default ({
  result,
  debugModeEnabled,
  onDeleteClick,
  onEditNoteClick,
}: {
  result: ErrorResult,
  debugModeEnabled: boolean,
  onDeleteClick?: (queryId: string) => void,
  onEditNoteClick?: (result: ErrorResult) => void,
}): Element<"div"> => {
  return (
    <div className="card border-danger mb-3">
      <div className="card-header border-danger bg-white">
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
        {onEditNoteClick && (
          <button
            type="button"
            className="btn btn-sm float-right"
            onClick={() => onEditNoteClick(result)}
          >
            <i className="far fa-comment-alt" aria-label="Set note"></i>
          </button>
        )}
        <CodeViewer statement={result.statement} />
      </div>
      <div className="card-body">
        <QueryNote note={result.note} />

        <h5 className="card-title">Query failed</h5>
        <p dangerouslySetInnerHTML={mdToHtml(result.error)} />

        <InfoView info={result.info} />

        <div className="btn-group my-2">
          <ShareButton result={result} />
          <DebugExport id={result.id} debugModeEnabled={debugModeEnabled} />
        </div>
      </div>
    </div>
  );
};
