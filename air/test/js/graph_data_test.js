import _ from "lodash";
import assert from "assert";

import {GraphData, GraphInfo} from "queries/graph_data";

const columnNames = prepper => _.map(prepper.traces("line"), trace => trace.name);

const xAxisValues = prepper => {
  const firstTrace = prepper.traces("line")[0];
  return firstTrace.x;
};

it("handles results without errors", () => {
  const rows = [{row: [0, 1, 2]}];
  const columns = ["x-axis", "col1", "col2"];
  const prepper = new GraphData(rows, columns);
  assert.deepEqual(["col1", "col2"], columnNames(prepper));
});

it("rows have distinct colours", () => {
  const rows = [{row: [0, 1, 2]}];
  const columns = ["x-axis", "col1", "col2"];
  const prepper = new GraphData(rows, columns);
  const colours = _.map(prepper.traces("bar"), trace => trace.line.color);
  assert.notEqual(colours[0], colours[1]);
});

it("pairs result and error", () => {
  const rows = [{row: [0, 1, 2]}];
  const columns = ["x-axis", "col", "col_noise"];
  const prepper = new GraphData(rows, columns);
  _.forEach(columnNames(prepper), name => assert.ok(_.startsWith(name, "col")));
  assert.equal(columnNames(prepper).length, 4); // col + 3 error traces
});

it("pairs result and error even when noise comes first", () => {
  const rows = [{row: [0, 1, 2]}];
  const columns = ["x-axis", "col_noise", "col"];
  const prepper = new GraphData(rows, columns);
  _.forEach(columnNames(prepper), name => assert.ok(_.startsWith(name, "col")));
  assert.equal(columnNames(prepper).length, 4); // col + 3 error traces
});

it("handles multiple val+noise with same name", () => {
  const rows = [{row: [0, 1, 2, 3, 4]}];
  const columns = ["x-axis", "col", "col_noise", "col", "col_noise"];
  const prepper = new GraphData(rows, columns);
  _.forEach(columnNames(prepper), name => assert.ok(_.startsWith(name, "col")));
  assert.equal(columnNames(prepper).length, 8); // 2 * (col + 3 error traces)
});

it("leaves un-matched noise value", () => {
  const rows = [{row: [0, 1]}];
  const columns = ["x-axis", "col_noise"];
  const prepper = new GraphData(rows, columns);
  assert.deepEqual(["col_noise"], columnNames(prepper));
});

it("retains column order for un-matched noise", () => {
  const rows = [{row: [0, 1, 2, 3, 4]}];
  const columns = ["x-axis", "val_noise", "col", "col_noise"];
  const prepper = new GraphData(rows, columns);
  const namePrefixes = _.map(columnNames(prepper), name => name.substring(0, 3));
  assert.deepEqual(["val", "col", "col", "col", "col"], namePrefixes);
});

it("produces a list of x-axis values", () => {
  const rows = [{row: [0, 1]}, {row: [2, 3]}];
  const columns = ["x-axis", "col"];
  const prepper = new GraphData(rows, columns);
  assert.deepEqual(xAxisValues(prepper), ["0", "2"]);
});

it("produces x-axis values of multiple values", () => {
  const rows = [{row: ["a", "b", 1]}, {row: ["c", "d", 2]}];
  const columns = ["l1", "l2", "col"];
  const prepper = new GraphData(rows, columns);
  assert.deepEqual(xAxisValues(prepper), ["a, b", "c, d"]);
});

it("produces x-axis values when column order is odd", () => {
  const rows = [{row: ["a", 1, "b"]}, {row: ["c", 2, "d"]}];
  const columns = ["l1", "l2", "col"];
  const prepper = new GraphData(rows, columns);
  assert.deepEqual(xAxisValues(prepper), ["a, b", "c, d"]);
});

it("produces traces for each y-value column", () => {
  const rows = [{row: ["a", 1, "b"]}, {row: ["c", 2, "d"]}];
  const columns = ["l1", "l2", "col"];
  const prepper = new GraphData(rows, columns);
  assert.equal(prepper.traces("bar").length, 1);
});

it("produces traces with the right mode", () => {
  const rows = [{row: ["a", 1]}, {row: ["b", 2]}];
  const columns = ["x", "y"];
  const prepper = new GraphData(rows, columns);
  _.forEach(prepper.traces("bar"), trace => assert.equal(trace.type, "bar"));
  _.forEach(prepper.traces("line"), trace => assert.equal(trace.type, "line"));
});

it("traces have the name of the column", () => {
  const rows = [{row: ["a", 1]}, {row: ["b", 2]}];
  const columns = ["x", "y"];
  const prepper = new GraphData(rows, columns);
  const trace = prepper.traces("bar")[0];
  assert.equal(trace.name, "y");
});

it("bar columns with noise data contain error information", () => {
  const rows = [{row: ["a", 1, 2]}, {row: ["b", 3, 4]}];
  const columns = ["x", "y", "y_noise"];
  const prepper = new GraphData(rows, columns);
  const trace = prepper.traces("bar")[0];
  assert.deepEqual(trace.error_y.array, [2, 4]);
});

it("line graphs with noise data contain error traces centered around real value for 1, 2, 3 SDs", () => {
  const rows = [{row: ["a", 1, 2]}, {row: ["b", 3, 4]}];
  const columns = ["x", "y", "y_noise"];
  const prepper = new GraphData(rows, columns);
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

it("can produce an x-axis label", () => {
  const rows = [{row: ["a", "b", 3]}, {row: ["c", "d", 6]}];
  const columns = ["x", "y", "z"];
  const prepper = new GraphData(rows, columns);
  assert.equal(prepper.xAxisLabel(), "x, y");
});

describe("GraphInfo", () => {
  it("lists all columns as candidates for x-axis", () => {
    const info = new GraphInfo(["col1", "col2"], [])
    assert.deepEqual(["col1", "col2"], info.xColumns());
  });

  describe("usableAsY", () => {
    it("is true for numeric columns", () => {
      const info = new GraphInfo(["col1", "col2", "col3"], [{row: [0, "something", 1.1]}])
      assert.ok(info.usableAsY("col1"))
      assert.ok(info.usableAsY("col3"))
    });

    it("is false otherwise", () => {
      const info = new GraphInfo(["col1", "col2", "col3"], [{row: [0, "something", 1.1]}])
      assert.equal(info.usableAsY("col2"), false)
    });
  });

  describe("chartable", () => {
    it("is true if there's at least one possible x and y axis", () => {
      const info = new GraphInfo(["col1", "col2", "col3"], [{row: [0, "something", 1.1]}, {row: [null, null, null]}]);
      assert.equal(info.chartable(), true);
    });

    it("is false otherwise", () => {
      const info = new GraphInfo(["col1", "col2", "col3"], [{row: ["a", "b", "c"]}, {row: [null, null, null]}]);
      assert.equal(info.chartable(), false);
    });

    it("can chart up to 1000 rows", () => {
      const rows = [];
      for (let i = 0; i < 1000; i++) {
        rows.push({row: ["a", 1]});
      }
      const columns = ["x", "y"];
      const info = new GraphInfo(columns, rows);
      assert.ok(info.chartable());
      rows.push({row: ["b", 2]}); // now we have 1001 lines which is over the limit
      const info2 = new GraphInfo(columns, rows);
      assert.equal(info2.chartable(), false);
    });

    it("is false if only one column", () => {
      const info = new GraphInfo(["col1"], [{row: [1]}, {row: [2]}]);
      assert.equal(info.chartable(), false);
    });

    it("is false if only one row", () => {
      const info = new GraphInfo(["x", "y", "z"], [{row: [1, 2, 3]}]);
      assert.equal(info.chartable(), false);
    });
  });
});
