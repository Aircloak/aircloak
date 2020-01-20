// @flow

import React from "react";

import {format} from "../queries/state";

const stateClasses = {
  completed: "label label-success",
  error: "label label-danger",
  cancelled: "label label-warning",
};

const stateClass = (state) => stateClasses[state] || "label label-info";

export const StateView = (props: {state: string}) => {
  const {state} = props;
  return <span className={stateClass(state)}>{format(state)}</span>;
}
