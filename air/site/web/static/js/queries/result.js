import React from "react";

export const Result = (props) =>
  <pre>
    {props.query}
  </pre>;

Result.propTypes = {
  query: React.PropTypes.string,
};
