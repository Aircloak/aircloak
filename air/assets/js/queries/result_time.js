// @flow

import React from "react";
import moment from "moment";

export default ({ time }: { time: string | number }) => {
  if (!time) {
    return null;
  }

  const inserted = moment.utc(time);
  return (
    <span
      className="result-time"
      title={inserted.local().format("YYYY-MM-DD HH:mm:ss")}
    >
      {inserted.fromNow()}
    </span>
  );
};
