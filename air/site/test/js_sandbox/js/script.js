function double_value(x) {
  return 2 * x;
}

function double_array(array) {
  for (var i = 0; i < array.length; i++) {
    array[i] *= 2;
  }
  return array;
}

function double_map(map) {
  for (var key in map) {
    map[key] *= 2;
  }
  return map;
}
