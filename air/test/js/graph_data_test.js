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
