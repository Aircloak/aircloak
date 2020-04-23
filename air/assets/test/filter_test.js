import assert from "assert";

import { filterColumns } from "../js/selectable_info/filter";

const column1 = { name: "name-one", type: "type_one", key_type: null };
const column2 = { name: "name-two", type: "type_two", key_type: "user_id" };
const column3 = {
  name: "name-three",
  type: "type_three",
  key_type: "account_id",
};
const columns = [column1, column2, column3];

const extractCols = (cols) =>
  cols.map((item) => ({
    name: item.name,
    type: item.type,
    key_type: item.key_type,
  }));

describe("filterColumns", () => {
  it("filters on name", () => {
    assert.deepEqual(
      extractCols(filterColumns("table", columns, "name-two")),
      extractCols([column2])
    );
  });

  it("filters on type", () => {
    assert.deepEqual(
      extractCols(filterColumns("table", columns, "type_two")),
      extractCols([column2])
    );
  });

  it("returns an empty array when there is no match", () => {
    assert.deepEqual(filterColumns("table", columns, "otherfoobar"), []);
  });

  it("returns the full array when all columns match", () => {
    assert.deepEqual(
      extractCols(extractCols(filterColumns("table", columns, "name"))),
      extractCols(columns)
    );
  });

  it("returns the full array when an empty filter is used", () => {
    assert.deepEqual(
      extractCols(filterColumns("table", columns, "")),
      extractCols(columns)
    );
  });
});
