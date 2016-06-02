import React from "react";

export const HistoryLoader = (props) => {
  if (props.historyLoaded) {
    return null;
  }

  if (props.historyLoading) {
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
  historyLoaded: React.PropTypes.bool,
  historyLoading: React.PropTypes.bool,
  handleLoadHistory: React.PropTypes.func,
};
