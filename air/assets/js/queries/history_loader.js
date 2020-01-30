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

export const HistoryLoader = ({ history, handleLoadHistory }: Props) => {
  if (history.error) {
    return (
      <div>
        <span className="label label-danger">Error</span>
        Loading history failed.{" "}
        <button
          className="btn btn-default"
          onClick={handleLoadHistory}
          type="button"
        >
          Retry loading previous queries
        </button>
      </div>
    );
  }

  if (history.loaded) {
    return null;
  }

  if (history.loading) {
    return (
      <p>
        <img
          alt="Icon indicating loading of past queries"
          src="/images/loader.gif"
        />{" "}
        loading history
      </p>
    );
  } else {
    return (
      <button
        type="button"
        className="btn btn-default"
        onClick={handleLoadHistory}
      >
        Load previous queries
      </button>
    );
  }
};
