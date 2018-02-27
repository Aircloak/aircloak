// @flow

import _ from "lodash";

export default <T>(items: Array<T>, key: (item: T) => any): Array<Array<T>> => {
  const result = [];
  let chunk = [];
  let lastKey = null;

  items.forEach((item) => {
    const nextKey = key(item);
    if (_.isNull(lastKey) || _.isEqual(nextKey, lastKey)) {
      chunk.push(item);
    } else {
      result.push(chunk);
      chunk = [item];
    }
    lastKey = nextKey;
  });

  if (!_.isEmpty(chunk)) {
    result.push(chunk);
  }

  return result;
};
