import _ from "lodash";
import assert from "assert";

import {GraphData, GraphInfo, GraphConfig} from "queries/graph_data";

describe("GraphData", () => {
  describe("ready", () => {
    it("is true when an x and y axis has been selected", () => {
      const data = new GraphData([], [], {xColumns: () => ["col1"], yColumns: () => ["col2"]})
      assert.equal(data.ready(), true);
    });

    it("is false when no x axis selected", () => {
      const data = new GraphData([], [], {xColumns: () => [], yColumns: () => ["col2"]})
      assert.equal(data.ready(), false);
    });

    it("is false when no y axis selected", () => {
      const data = new GraphData([], [], {xColumns: () => ["col1"], yColumns: () => []})
      assert.equal(data.ready(), false);
    });
  });

  describe("x", () => {
    it("is all possible values for a single column", () => {
      const data = new GraphData(
        ["col1", "col2"],
        [{row: [null, "foo"]}, {row: [null, "bar"]}],
        {xColumns: () => ["col2"], yColumns: () => []}
      )
      assert.deepEqual(data.x(), ["foo", "bar"]);
    });

    it("is all possible combinations for a multiple", () => {
      const data = new GraphData(
        ["col1", "col2", "col3"],
        [{row: [null, "foo", 2]}, {row: [null, "bar", 3]}],
        {xColumns: () => ["col2", "col3"], yColumns: () => []}
      )
      assert.deepEqual(data.x(), ["foo, 2", "bar, 3"]);
    });

    it("transforms the values through the value formatter if provided", () => {
      const data = new GraphData(
        ["col1", "col2"],
        [{row: [1, 2]}, {row: [3, 4]}],
        {xColumns: () => ["col1", "col2"], yColumns: () => []},
        (value) => `formatted-${value}`
      );
      assert.deepEqual(data.x(), ["formatted-1, formatted-2", "formatted-3, formatted-4"]);
    });
  });

  describe("series", () => {
    it("works for single columns", () => {
      const data = new GraphData(
        ["col1", "col2", "col3"],
        [{row: [null, "foo", 2]}, {row: [null, "bar", 3]}],
        {xColumns: () => [], yColumns: () => ["col3"]}
      )
      assert.deepEqual(data.series(), [{label: "col3", data: [2, 3]}]);
    });

    it("works for multiple y columns", () => {
      const data = new GraphData(
        ["col1", "col2", "col3"],
        [{row: [null, "foo", 2]}, {row: [null, "bar", 3]}],
        {xColumns: () => [], yColumns: () => ["col1", "col3"]}
      )
      assert.deepEqual(data.series(), [{label: "col1", data: [null, null]}, {label: "col3", data: [2, 3]}]);
    });
  });
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

describe("GraphConfig", () => {
  it("does not include any column by default", () => {
    const config = new GraphConfig();
    assert.deepEqual(config.xColumns(), []);
    assert.deepEqual(config.yColumns(), []);
  })

  it("can add x columns", () => {
    const config = new GraphConfig();
    config.addX("col1");
    config.addX("col2");
    assert.deepEqual(config.xColumns(), ["col1", "col2"]);
    assert.deepEqual(config.yColumns(), []);
  });

  it("can add y columns", () => {
    const config = new GraphConfig();
    config.addY("col1");
    config.addY("col2");
    assert.deepEqual(config.xColumns(), []);
    assert.deepEqual(config.yColumns(), ["col1", "col2"]);
  });

  it("can move columns from x to y", () => {
    const config = new GraphConfig();
    config.addX("col1");
    config.addX("col2");
    config.addY("col2");
    assert.deepEqual(config.xColumns(), ["col1"]);
    assert.deepEqual(config.yColumns(), ["col2"]);
  });

  it("can move columns from y to x", () => {
    const config = new GraphConfig();
    config.addY("col1");
    config.addY("col2");
    config.addX("col2");
    assert.deepEqual(config.xColumns(), ["col2"]);
    assert.deepEqual(config.yColumns(), ["col1"]);
  });

  it("can remove columns", () => {
    const config = new GraphConfig();
    config.addX("col1");
    config.remove("col1");
    assert.deepEqual(config.xColumns(), []);
    assert.deepEqual(config.yColumns(), []);
  });

  it("returns this from mutators", () => {
    const config = new GraphConfig();
    assert.deepEqual(config.addX("col1").addY("col2").remove("col2").xColumns(), ["col1"]);
  });
});
