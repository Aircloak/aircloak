import _ from "lodash";
import assert from "assert";

import {GraphData} from "queries/graph_data";

it("handles results without errors", () => {
  const rows = [{row: [0, 1, 2]}];
  const columns = ["x-axis", "col1", "col2"];
  const prepper = new GraphData(rows, columns)
  const results = prepper.yColumns();
  assert.equal(results[0].name, "col1");
  assert.equal(results[0].index, 1);
  assert.equal(results[1].name, "col2");
  assert.equal(results[1].index, 2);
});

it("rows have distinct colours", () => {
  const rows = [{row: [0, 1, 2]}];
  const columns = ["x-axis", "col1", "col2"];
  const prepper = new GraphData(rows, columns)
  const results = prepper.yColumns();
  assert.notEqual(results[0].colour, results[1].colour);
});

it("pairs result and error", () => {
  const rows = [{row: [0, 1, 2]}];
  const columns = ["x-axis", "col", "col_noise"];
  const prepper = new GraphData(rows, columns)
  const result = prepper.yColumns()[0];
  assert.equal(result.index, 1);
  assert.equal(result.noise.index, 2);
});

it("pairs result and error even when noise comes first", () => {
  const rows = [{row: [0, 1, 2]}];
  const columns = ["x-axis", "col_noise", "col"];
  const prepper = new GraphData(rows, columns)
  const result = prepper.yColumns()[0];
  assert.equal(result.index, 2);
  assert.equal(result.noise.index, 1);
});

it("handles multiple val+noise with same name", () => {
  const rows = [{row: [0, 1, 2, 3, 4]}];
  const columns = ["x-axis", "col", "col_noise", "col", "col_noise"];
  const prepper = new GraphData(rows, columns)
  const results = prepper.yColumns();
  assert.equal(results[0].index, 1);
  assert.equal(results[0].noise.index, 2);
  assert.equal(results[1].index, 3);
  assert.equal(results[1].noise.index, 4);
});

it("leaves un-matched noise value", () => {
  const rows = [{row: [0, 1]}];
  const columns = ["x-axis", "col_noise"];
  const prepper = new GraphData(rows, columns)
  const result = prepper.yColumns()[0];
  assert.equal(result.index, 1);
  assert.equal(result.name, "col_noise");
});

it("retains column order for un-matched noise", () => {
  const rows = [{row: [0, 1, 2, 3, 4]}];
  const columns = ["x-axis", "val_noise", "col", "col_noise"];
  const prepper = new GraphData(rows, columns)
  const results = prepper.yColumns();
  assert.equal(results[0].index, 1);
  assert.equal(results[0].name, "val_noise");
  assert.equal(results[1].index, 2);
  assert.equal(results[1].noise.index, 3);
});

it("produces a list of x-axis values", () => {
  const rows = [{row: [0, 1]}, {row: [2, 3]}];
  const columns = ["x-axis", "col"];
  const prepper = new GraphData(rows, columns)
  assert.deepEqual(prepper.xAxisValues(), ["0", "2"]);
});

it("produces x-axis values of multiple values", () => {
  const rows = [{row: ["a", "b", 1]}, {row: ["c", "d", 2]}];
  const columns = ["l1", "l2", "col"];
  const prepper = new GraphData(rows, columns)
  assert.deepEqual(prepper.xAxisValues(), ["a, b", "c, d"]);
});

it("produces x-axis values when column order is odd", () => {
  const rows = [{row: ["a", 1, "b"]}, {row: ["c", 2, "d"]}];
  const columns = ["l1", "l2", "col"];
  const prepper = new GraphData(rows, columns)
  assert.deepEqual(prepper.xAxisValues(), ["a, b", "c, d"]);
});

it("produces traces for each y-value column", () => {
  const rows = [{row: ["a", 1, "b"]}, {row: ["c", 2, "d"]}];
  const columns = ["l1", "l2", "col"];
  const prepper = new GraphData(rows, columns)
  assert.equal(prepper.traces("bar").length, 1);
});

it("produces traces with the right mode", () => {
  const rows = [{row: ["a", 1]}, {row: ["b", 2]}];
  const columns = ["x", "y"];
  const prepper = new GraphData(rows, columns)
  _.forEach(prepper.traces("bar"), trace => assert.equal(trace.type, "bar"));
  _.forEach(prepper.traces("line"), trace => assert.equal(trace.type, "line"));
});

it("traces have the name of the column", () => {
  const rows = [{row: ["a", 1]}, {row: ["b", 2]}];
  const columns = ["x", "y"];
  const prepper = new GraphData(rows, columns)
  const trace = prepper.traces("bar")[0];
  assert.equal(trace.name, "y");
});

it("traces have valid x-axis values", () => {
  const rows = [{row: ["a", 1]}, {row: ["b", 2]}];
  const columns = ["x", "y"];
  const prepper = new GraphData(rows, columns)
  const trace = prepper.traces("bar")[0];
  assert.deepEqual(trace.x, prepper.xAxisValues());
});

it("bar columns with noise data contain error information", () => {
  const rows = [{row: ["a", 1, 2]}, {row: ["b", 3, 4]}];
  const columns = ["x", "y", "y_noise"];
  const prepper = new GraphData(rows, columns)
  const trace = prepper.traces("bar")[0];
  assert.deepEqual(trace.error_y.array, [2, 4]);
});

it("line graphs with noise data contain error traces centered around real value for 1, 2, 3 SDs", () => {
  const rows = [{row: ["a", 1, 2]}, {row: ["b", 3, 4]}];
  const columns = ["x", "y", "y_noise"];
  const prepper = new GraphData(rows, columns)
  const errorTrace1 = prepper.traces("line")[0];
  const errorTrace2 = prepper.traces("line")[1];
  const errorTrace3 = prepper.traces("line")[2];
  const lineTrace = prepper.traces("line")[3];
  assert.ok(_.startsWith(errorTrace1.name, "y noise"));
  assert.ok(_.startsWith(errorTrace2.name, "y noise"));
  assert.ok(_.startsWith(errorTrace3.name, "y noise"));
  assert.equal(lineTrace.name, "y");
  assert.deepEqual(errorTrace1.y, [7, 15, -9, -5]);
  assert.deepEqual(errorTrace2.y, [5, 11, -5, -3]);
  assert.deepEqual(errorTrace3.y, [3, 7, -1, -1]);
});

it("can chart if an x and a y column", () => {
  const rows = [{row: ["a", 1]}, {row: ["b", 2]}];
  const columns = ["x", "y"];
  const prepper = new GraphData(rows, columns)
  assert.ok(prepper.charteable());
});

it("can chart up to 1000 rows", () => {
  const rows = [];
  for (var i = 0; i < 1000; i++) {
    rows.push({row: ["a", 1]});
  }
  const columns = ["x", "y"];
  const prepper = new GraphData(rows, columns)
  assert.ok(prepper.charteable());
  rows.push({row: ["b", 2]}); // now we have 1001 lines which is over the limit
  const prepper2 = new GraphData(rows, columns)
  assert.equal(prepper2.charteable(), false);
});

it("not charteable if only one column", () => {
  const rows = [{row: [1]}, {row: [2]}];
  const columns = ["x"];
  const prepper = new GraphData(rows, columns)
  assert.equal(prepper.charteable(), false);
});

it("not charteable if only one row", () => {
  const rows = [{row: [1, 2, 3]}];
  const columns = ["x", "y", "z"];
  const prepper = new GraphData(rows, columns)
  assert.equal(prepper.charteable(), false);
});

it("not charteable if none of the columns are numerical", () => {
  const rows = [{row: ["a", "b"]}, {row: ["c", "d"]}];
  const columns = ["x", "y"];
  const prepper = new GraphData(rows, columns)
  assert.equal(prepper.charteable(), false);
});
