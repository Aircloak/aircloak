import assert from "assert";

import {Filter} from "selectable_info/filter";

const column1 = {name: "name1", type: "type1", user_id: false};
const column2 = {name: "name2", type: "type2", user_id: true};
const columns = [column1, column2];

it("constructs successfully", () => {
  assert.doesNotThrow(() => new Filter("test"));
  assert.doesNotThrow(() => new Filter(""));
});

describe("anyColumnMatches", () => {
  it("true for column name match", () => {
    const filter = new Filter("name");
    assert(filter.anyColumnMatches(columns));
  });

  it("true for column type match", () => {
    const filter = new Filter("type");
    assert(filter.anyColumnMatches(columns));
  });

  it("false for not matching on type or name", () => {
    const filter = new Filter("other");
    assert.notEqual(filter.anyColumnMatches(columns), true);
  });

  it("true when incomplete regex", () => {
    const filter = new Filter("name(");
    assert(filter.anyColumnMatches(columns));
  });
});

describe("filterColumns", () => {
  it("filters on name", () => {
    const filter = new Filter("name1");
    assert.deepEqual(filter.filterColumns(columns), [column1]);
  });

  it("filters on type", () => {
    const filter = new Filter("type1");
    assert.deepEqual(filter.filterColumns(columns), [column1]);
  });

  it("empty array for no match", () => {
    const filter = new Filter("other");
    assert.deepEqual(filter.filterColumns(columns), []);
  });

  it("full array when all match", () => {
    const filter = new Filter("name");
    assert.deepEqual(filter.filterColumns(columns), columns);
  });

  it("full array when incomplete regex", () => {
    const filter = new Filter("name(");
    assert.deepEqual(filter.filterColumns(columns), columns);
  });
});
