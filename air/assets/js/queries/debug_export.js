// @flow

import React from "react";

export default ({
  id,
  debugModeEnabled
}: {
  id: string,
  debugModeEnabled: boolean
}) => {
  if (debugModeEnabled) {
    return (
      <a
        className="btn btn-default btn-xs"
        href={`/queries/${id}/debug_export`}
      >
        Download debug export
      </a>
    );
  } else {
    return null;
  }
};
