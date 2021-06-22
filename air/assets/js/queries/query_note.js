// @flow
import type { Node } from "react";
import React from "react";

type QueryNoteProps = {
  note: string | null,
};

export default ({ note }: QueryNoteProps): Node | null => {
  if (!note) {
    return null;
  }

  return (
    <div className="alert alert-info">
      <i className="fas fa-comment-alt" aria-label="Query note"></i>
      <span className="ml-2">{note}</span>
    </div>
  );
};
