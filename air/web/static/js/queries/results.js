import React from "react";

import {Result} from "./result";
import {PendingResult} from "./pending_result";
import {Error} from "./error";

export const Results = (props) =>
  <div>
    {props.results.map((result, i) => {
      if (result.pendingResult) {
        return <PendingResult key={i} {...result} />;
      } else if (result.columns) {
        return <Result key={i} {...result} {...props} />;
      } else {
        return <Error key={i} {...result} />;
      }
    })}
  </div>;

Results.propTypes = {
  results: React.PropTypes.array.isRequired,
  handleLoadRows: React.PropTypes.func,
  handleLessRows: React.PropTypes.func,
};
