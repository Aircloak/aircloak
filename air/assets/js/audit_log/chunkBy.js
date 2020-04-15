// @flow

export default <T>(
  items: Array<T>,
  key: (item: T) => string
): Array<Array<T>> => {
  const result = [];
  let chunk = [];
  let lastKey = null;

  items.forEach((item) => {
    const nextKey = key(item);
    if (lastKey === null || nextKey === lastKey) {
      chunk.push(item);
    } else {
      result.push(chunk);
      chunk = [item];
    }
    lastKey = nextKey;
  });

  if (chunk.length > 0) {
    result.push(chunk);
  }

  return result;
};
