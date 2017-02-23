// @flow

import React from "react";

const stateClasses = {
  "completed": "label label-success",
  "error": "label label-danger",
  "cancelled": "label label-warning",
};

const stateClass = (state) => stateClasses[state] || "label label-info";

export const StateView = (props: {state: string}) =>
  <span className={stateClass(props.state)}>{props.state}</span>;
