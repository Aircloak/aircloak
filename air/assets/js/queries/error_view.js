// @flow
import type { Element } from "React"; /* eslint-disable react/no-danger */

import React from "react";
import pagedown from "pagedown";

import CodeViewer from "../code_viewer";
import InfoView from "./info_view";
import DebugExport from "./debug_export";
import ShareButton from "./share_button";
import NoteButton from "./note_button";
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
  updateNote,
}: {
  result: ErrorResult,
  debugModeEnabled: boolean,
  onDeleteClick?: (queryId: string) => void,
  updateNote?: (id: string, note: string | null) => void,
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
        {updateNote && (
          <NoteButton
            initialValue={result.note}
            onChange={(newNote) => updateNote(result.id, newNote)}
          />
        )}
        <CodeViewer statement={result.statement} />
      </div>
      <div className="card-body">
        <QueryNote id={result.id} note={result.note} updateNote={updateNote} />

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
