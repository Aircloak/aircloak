// @flow

import React, { useState, useEffect } from "react";
import moment from "moment";

function useRelativeTime(time: string | number) {
  const [relativeTime, setRelativeTime] = useState(() =>
    moment.utc(time).fromNow()
  );

  useEffect(() => {
    const id = setInterval(() => {
      setRelativeTime(moment.utc(time).fromNow());
    }, 5000);

    return () => clearInterval(id);
  }, [time]);

  return relativeTime;
}

export default ({ time }: { time: string | number }) => {
  const relative = useRelativeTime(time);
  const inserted = moment.utc(time);
  return (
    <time
      className="small text-muted ml-1"
      dateTime={inserted.toISOString()}
      title={inserted.local().format("YYYY-MM-DD HH:mm:ss")}
    >
      {relative}
    </time>
  );
};
