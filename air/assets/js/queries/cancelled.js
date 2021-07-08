// @flow

import type { Element } from "React";
import React from "react";

import CodeViewer from "../code_viewer";
import DebugExport from "./debug_export";
import ShareButton from "./share_button";
import NoteButton from "./note_button";
import ResultTime from "./result_time";
import QueryNote from "./query_note";

import type { CancelledResult } from "./result";

export default ({
  result,
  debugModeEnabled,
  showDataSource,
  onDeleteClick,
  updateNote,
}: {
  result: CancelledResult,
  debugModeEnabled: boolean,
  showDataSource?: boolean,
  onDeleteClick?: (queryId: string) => void,
  updateNote?: (id: string, note: string | null) => void,
}): Element<"div"> => {
  const dataSource = result.data_source.name;
  return (
    <div className="card border-warning mb-3">
      <div className="card-header border-warning bg-white">
        <ResultTime time={result.inserted_at} />
        {showDataSource && (
          <span className="small text-muted">
            {" · "}
            <a className="text-muted" href={`/data_sources/${dataSource}`}>
              {dataSource}
            </a>
          </span>
        )}
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
        <h5 className="card-title">Query cancelled</h5>
        <div className="btn-group my-2">
          <ShareButton result={result} />
          <DebugExport id={result.id} debugModeEnabled={debugModeEnabled} />
        </div>
      </div>
    </div>
  );
};
