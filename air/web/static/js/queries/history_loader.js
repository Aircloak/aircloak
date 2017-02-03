// @flow

import React from "react";

export type History = {
  before: string,
  loaded: boolean,
  loading: boolean,
  error?: boolean
};

type Props = {
  history: History,
  handleLoadHistory: () => void
};

export const HistoryLoader = (props: Props) => {
  if (props.history.error) {
    return (
      <div>
        <span className="label label-danger">Error</span> Loading history failed.&nbsp;
        <button className="btn btn-default" onClick={props.handleLoadHistory}>
          Retry loading previous queries
        </button>
      </div>
    );
  }

  if (props.history.loaded) {
    return null;
  }

  if (props.history.loading) {
    return (<p><img role="presentation" src="/images/loader.gif" /> loading history</p>);
  } else {
    return (
      <button className="btn btn-default" onClick={props.handleLoadHistory}>
        Load previous queries
      </button>
    );
  }
};
