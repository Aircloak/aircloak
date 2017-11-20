import assert from "assert";

import {GraphData, GraphInfo, GraphConfig} from "queries/graph_data";

describe("GraphData", () => {
  describe("ready", () => {
    it("is true when an x and y axis has been selected", () => {
      const data = new GraphData([], [], {xColumns: () => [0], yColumns: () => [1]});
      assert.equal(data.ready(), true);
    });

    it("is false when no x axis selected", () => {
      const data = new GraphData([], [], {xColumns: () => [], yColumns: () => [2]});
      assert.equal(data.ready(), false);
    });

    it("is false when no y axis selected", () => {
      const data = new GraphData([], [], {xColumns: () => [0], yColumns: () => []});
      assert.equal(data.ready(), false);
    });
  });

  describe("x", () => {
    it("is all possible values for a single column", () => {
      const data = new GraphData(
        ["col1", "col2"],
        [{row: [null, "foo"]}, {row: [null, "bar"]}],
        {xColumns: () => [1], yColumns: () => []}
      );
      assert.deepEqual(data.x(), ["foo", "bar"]);
    });

    it("is all possible combinations for a multiple", () => {
      const data = new GraphData(
        ["col1", "col2", "col3"],
        [{row: [null, "foo", 2]}, {row: [null, "bar", 3]}],
        {xColumns: () => [1, 2], yColumns: () => []}
      );
      assert.deepEqual(data.x(), ["foo, 2", "bar, 3"]);
    });

    it("transforms the values through the value formatter if provided", () => {
      const data = new GraphData(
        ["col1", "col2"],
        [{row: [1, 2]}, {row: [3, 4]}],
        {xColumns: () => [0, 1], yColumns: () => []},
        (value) => `formatted-${value}`
      );
      assert.deepEqual(data.x(), ["formatted-1, formatted-2", "formatted-3, formatted-4"]);
    });
  });

  describe("xLabel", () => {
    it("is the name of the column when single column selected", () => {
      const data = new GraphData(["col1", "col2"], [], {xColumns: () => [0], yColumns: () => []});
      assert.equal(data.xLabel(), "col1");
    });

    it("is comma-joined names of columns when multiple columns selected", () => {
      const data = new GraphData(["col1", "col2", "col3"], [], {xColumns: () => [0, 2], yColumns: () => []});
      assert.equal(data.xLabel(), "col1, col3");
    });
  });

  describe("series", () => {
    it("works for single columns", () => {
      const data = new GraphData(
        ["col1", "col2", "col3"],
        [{row: [null, "foo", 2]}, {row: [null, "bar", 3]}],
        {xColumns: () => [], yColumns: () => [2]}
      );
      assert.deepEqual(data.series(), [{label: "col3", data: [2, 3], index: 2}]);
    });

    it("works for multiple y columns", () => {
      const data = new GraphData(
        ["col1", "col2", "col3"],
        [{row: [null, "foo", 2]}, {row: [null, "bar", 3]}],
        {xColumns: () => [], yColumns: () => [0, 2]}
      );
      assert.deepEqual(data.series(),
        [{label: "col1", data: [null, null], index: 0}, {label: "col3", data: [2, 3], index: 2}]);
    });
  });
});

describe("GraphInfo", () => {
  it("lists all columns as candidates for x-axis", () => {
    const info = new GraphInfo(["col1", "col2"], []);
    assert.deepEqual(["col1", "col2"], info.xColumns());
  });

  describe("usableAsY", () => {
    it("is true for numeric columns", () => {
      const info = new GraphInfo(["col1", "col2", "col3"], [{row: [0, "something", 1.1]}]);
      assert.ok(info.usableAsY(0));
      assert.ok(info.usableAsY(2));
    });

    it("is false otherwise", () => {
      const info = new GraphInfo(["col1", "col2", "col3"], [{row: [0, "something", 1.1]}]);
      assert.equal(info.usableAsY(1), false);
    });
  });

  describe("chartable", () => {
    it("is true if there's at least one possible x and y axis", () => {
      const info = new GraphInfo(
        ["col1", "col2", "col3"],
        [{row: [0, "something", 1.1]}, {row: [null, null, null]}]
      );
      assert.equal(info.chartable(), true);
    });

    it("is false otherwise", () => {
      const info = new GraphInfo(
        ["col1", "col2", "col3"],
        [{row: ["a", "b", "c"]}, {row: [null, null, null]}]
      );
      assert.equal(info.chartable(), false);
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
  });

  it("can add x columns", () => {
    const config = new GraphConfig();
    config.addX("col1");
    config.addX("col2");
    assert.deepEqual(config.xColumns(), ["col1", "col2"]);
    assert.deepEqual(config.yColumns(), []);
  });

  it("doesn't matter in what order you add columns", () => {
    const config1 = new GraphConfig().
      addX("col1").
      addX("col2").
      addY("col3").
      addY("col4");
    const config2 = new GraphConfig().
      addX("col2").
      addX("col1").
      addY("col4").
      addY("col3");

    assert.deepEqual(config1.xColumns(), config2.xColumns());
    assert.deepEqual(config1.yColumns(), config2.yColumns());
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
    assert.deepEqual(
      config.
        addX("col1").
        addY("col2").
        remove("col2").
        xColumns(),
      ["col1"]
    );
  });
});
