// @flow
import type { Node } from "react";
import React from "react";
import NoteButton from "./note_button";

type QueryNoteProps = {
  id: string,
  note: string | null,
  updateNote?: (id: string, note: string | null) => void,
};

export default ({ id, note, updateNote }: QueryNoteProps): Node | null => {
  if (!note) {
    return null;
  }

  return (
    <div className="alert alert-info">
      <i className="fas fa-comment-alt" aria-label="Query note"></i>
      {updateNote && (
        <>
          <button
            type="button"
            className="mt-n1 btn float-right"
            onClick={() => {
              if (window.confirm("Remove query note?")) {
                updateNote(id, null);
              }
            }}
          >
            <i className="fas fa-times" aria-label="Remove"></i>
          </button>
          <NoteButton
            initialValue={note}
            onChange={(newNote) => updateNote(id, newNote)}
          />
        </>
      )}
      <span className="ml-2">{note}</span>
    </div>
  );
};
