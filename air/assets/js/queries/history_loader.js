// @flow

import type { Element } from "React";
import React from "react";
import loader from "../../static/images/loader.gif";

export type History = {
  before: string,
  loaded: boolean,
  loading: boolean,
  error?: boolean,
};

type Props = {
  history: History,
  handleLoadHistory: () => void,
};

export const HistoryLoader = ({
  history,
  handleLoadHistory,
}: Props): null | Element<"button"> | Element<"div"> | Element<"p"> => {
  if (history.error) {
    return (
      <div>
        <span className="badge badge-danger">Error</span>
        Loading history failed.{" "}
        <button
          className="btn btn-secondary"
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
        <img alt="Icon indicating loading of past queries" src={loader} />{" "}
        loading history
      </p>
    );
  } else {
    return (
      <button
        type="button"
        id="LoadHistory"
        className="btn btn-secondary"
        onClick={handleLoadHistory}
      >
        Load previous queries
      </button>
    );
  }
};
