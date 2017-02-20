// @flow

import React from "react";

type QueryState = {
  state: string,
};

export const StateView = (props: QueryState) => {
  if (props.state === "completed") {
    return (<span className="label label-success">{props.state}</span>);
  } else {
    return (<span className="label label-info">{props.state}</span>);
  }
};
