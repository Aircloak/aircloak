// @flow

import React from "react";

export const DebugExport = (props: {id: string, debugModeEnabled: boolean}) => {
  if (props.debugModeEnabled) {
    return (
      <a className="btn btn-default btn-xs" href={`/queries/${props.id}/debug_export`}>
        Download debug export
      </a>
    );
  } else {
    return null;
  }
};
