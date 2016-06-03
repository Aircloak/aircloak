import React from "react";

export const HistoryLoader = (props) => {
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

HistoryLoader.propTypes = {
  history: React.PropTypes.shape({
    loaded: React.PropTypes.bool.isRequired,
    loading: React.PropTypes.bool.isRequired,
    error: React.PropTypes.bool,
  }),
  handleLoadHistory: React.PropTypes.func,
};
