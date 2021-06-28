// @flow
import type { Node } from "react";
import React from "react";

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
        <button
          type="button"
          className="mt-n1 btn btn-sm float-right"
          onClick={() => {
            if (window.confirm("Remove query note?")) {
              updateNote(id, null);
            }
          }}
        >
          <i className="fas fa-times" aria-label="Remove"></i>
        </button>
      )}
      <span className="ml-2">{note}</span>
    </div>
  );
};
