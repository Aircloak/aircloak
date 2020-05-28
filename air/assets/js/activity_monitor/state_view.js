// @flow

import React from "react";

import { format } from "../queries/state";

const stateClasses = {
  completed: "badge badge-success",
  error: "badge badge-danger",
  cancelled: "badge badge-warning",
};

const stateClass = (queryState) =>
  stateClasses[queryState] || "label label-info";

export default ({ queryState }: { queryState: string }) => {
  return <span className={stateClass(queryState)}>{format(queryState)}</span>;
};
