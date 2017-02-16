// @flow

import React from "react";

export type State = {
  state: string,
};

export const StateView = (props: State) => {
  if (props.state === "completed") {
    return (<span className="label label-success">{props.state}</span>);
  } else {
    return (<span className="label label-info">{props.state}</span>);
  }
};
