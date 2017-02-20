// @flow

import React from "react";

type QueryState = {
  state: string,
};

export const StateView = (props: QueryState) => {
  if (props.state === "completed") {
    return (<span className="label label-success">{props.state}</span>);
  } else if (props.state === "cancelled") {
    return (<span className="label label-warning">{props.state}</span>);
  } else if (props.state === "error") {
    return (<span className="label label-danger">{props.state}</span>);
  } else {
    return (<span className="label label-info">{props.state}</span>);
  }
};
