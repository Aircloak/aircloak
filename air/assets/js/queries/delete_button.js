// @flow

import React from "react";

export default ({
  id,
  onClick,
}: {
  id: string,
  onClick?: (id: string) => void,
}) => {
  if (onClick) {
    return (
      <button
        type="button"
        className="btn btn-danger btn-sm"
        onClick={() => onClick(id)}
      >
        Delete
      </button>
    );
  } else {
    return null;
  }
};
