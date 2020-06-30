// @flow

import React from "react";
import moment from "moment";

export default ({ time }: { time: string | number }) => {
  if (!time) {
    return null;
  }

  const inserted = moment.utc(time);
  return (
    <time
      className="small text-muted ml-1"
      datetime={inserted.toISOString()}
      title={inserted.local().format("YYYY-MM-DD HH:mm:ss")}
    >
      {inserted.fromNow()}
    </time>
  );
};
