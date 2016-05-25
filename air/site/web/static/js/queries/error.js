import React from "react";

export const Error = (props) =>
  <div>
    <pre>{props.query}</pre>
    <div className="alert alert-danger">{props.error}</div>
  </div>;

Error.propTypes = {
  query: React.PropTypes.string,
  error: React.PropTypes.string,
};
