// @flow

import React from "react";

export const DebugExport = (props: {id: string, debugModeEnabled: boolean}) => {
  const {debugModeEnabled, id} = props;
  if (debugModeEnabled) {
    return (
      <a className="btn btn-default btn-xs" href={`/queries/${id}/debug_export`}>
        Download debug export
      </a>
    );
  } else {
    return null;
  }
};
