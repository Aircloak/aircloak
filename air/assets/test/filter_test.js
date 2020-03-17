import assert from "assert";

import { Filter, EmptyFilter } from "../js/selectable_info/filter";

const column1 = { name: "name1", type: "type1", key_type: null };
const column2 = { name: "name2", type: "type2", key_type: "user_id" };
const column3 = { name: "name3", type: "type3", key_type: "account_id" };
const columns = [column1, column2, column3];

it("constructs successfully", () => {
  assert.doesNotThrow(() => new Filter(/test/));
});

describe("anyColumnMatches", () => {
  it("is true for column name match", () => {
    const filter = new Filter(/name/);
    assert(filter.anyColumnMatches(columns));
  });

  it("is true for column type match", () => {
    const filter = new Filter(/type/);
    assert(filter.anyColumnMatches(columns));
  });

  it("is false for not matching on type or name", () => {
    const filter = new Filter(/other/);
    assert.notEqual(filter.anyColumnMatches(columns), true);
  });

  it("is true for empty filters ", () => {
    const filter = new EmptyFilter();
    assert(filter.anyColumnMatches(columns));
  });
});

describe("filterColumns", () => {
  it("filters on name", () => {
    const filter = new Filter(/name1/);
    assert.deepEqual(filter.filterColumns(columns), [column1]);
  });

  it("filters on type", () => {
    const filter = new Filter(/type1/);
    assert.deepEqual(filter.filterColumns(columns), [column1]);
  });

  it("returns an empty array when there is no match", () => {
    const filter = new Filter(/other/);
    assert.deepEqual(filter.filterColumns(columns), []);
  });

  it("returns the full array when all columns match", () => {
    const filter = new Filter(/name/);
    assert.deepEqual(filter.filterColumns(columns), columns);
  });

  it("returns the full array when an empty filter is used", () => {
    const filter = new EmptyFilter();
    assert.deepEqual(filter.filterColumns(columns), columns);
  });

  it("allows filters using regex capabilities", () => {
    const filter = new Filter(/name(1|3)/);
    assert.deepEqual(filter.filterColumns(columns), [column1, column3]);
  });
});
