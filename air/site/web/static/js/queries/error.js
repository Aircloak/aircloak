import React from "react";

export const Error = (props) =>
  <div>
    <pre>{props.statement}</pre>
    <div className="alert alert-danger">{props.error}</div>
  </div>;

Error.propTypes = {
  statement: React.PropTypes.string,
  error: React.PropTypes.string,
};
