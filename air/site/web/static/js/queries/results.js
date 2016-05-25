import React from "react";

import {Result} from "./result";
import {Error} from "./error";

export const Results = (props) =>
  <div>
    {props.results.map((result, i) => {
      if (result.columns) {
        return <Result key={i} {...result} />;
      } else {
        return <Error key={i} {...result} />;
      }
    })}
  </div>;

Results.propTypes = {
  results: React.PropTypes.array.isRequired,
};
