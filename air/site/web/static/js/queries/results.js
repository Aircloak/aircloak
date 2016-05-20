import React from "react";

import {Result} from "./result";

export const Results = (props) =>
  <div>
    {props.results.map((result, i) =>
      <Result key={i} {...result} />
    )}
  </div>;

Results.propTypes = {
  results: React.PropTypes.array.isRequired,
};
