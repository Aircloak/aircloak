// @flow

import React from "react";

import { format } from "../queries/state";

const stateClasses = {
  completed: "label label-success",
  error: "label label-danger",
  cancelled: "label label-warning"
};

const stateClass = queryState => stateClasses[queryState] || "label label-info";

export default ({ queryState }: { queryState: string }) => {
  return <span className={stateClass(queryState)}>{format(queryState)}</span>;
};
