// @flow

import _ from "lodash";

export default (events, key) => {
  const result = [];
  let chunk = [];
  let lastKey = null;

  events.forEach((event) => {
    const nextKey = key(event);
    if (lastKey === null || nextKey === lastKey) {
      chunk.push(event);
    } else {
      result.push(chunk);
      chunk = [event];
    }
    lastKey = nextKey;
  })

  if (!_.isEmpty(chunk)) {
    result.push(chunk);
  }

  return result;
}
