// @flow

import type { Element } from "React";
import React from "react";

export default ({
  id,
  debugModeEnabled,
}: {
  id: string,
  debugModeEnabled: boolean,
}): null | Element<"a"> => {
  if (debugModeEnabled) {
    return (
      <a
        className="btn btn-outline-secondary btn-sm"
        href={`/queries/${id}/debug_export`}
      >
        <i className="fas fa-file-medical-alt"></i> Download debug export
      </a>
    );
  } else {
    return null;
  }
};
