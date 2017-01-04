import _ from "lodash";
import assert from "assert";

import completions from "code_editor/completion";

it("completes keywords", () => {
  assert.deepEqual(
    completions("thing cou| rest", 9, _.identity, [], []),
    {
      list: [
        {text: "COUNT(distinct columnName)", from: 6, to: 9},
        {text: "COUNT(columnName)", from: 6, to: 9},
        {text: "COUNT(*)", from: 6, to: 9},
      ],
      from: 6,
      to: 9,
    }
  );
});

it("completes expressions", () => {
  assert.deepEqual(
    completions("show tab| rest", 8, _.identity, [], []),
    {
      list: [{text: "SHOW TABLES;", from: 0, to: 8}],
      from: 0,
      to: 8,
    }
  );
});

it("completes 'show columns from <table>'", () => {
  assert.deepEqual(
    completions("show col| rest", 8, _.identity, ["table1", "table2"], ["a column"]),
    {
      list: [
        {text: "SHOW COLUMNS FROM table1;", from: 0, to: 8},
        {text: "SHOW COLUMNS FROM table2;", from: 0, to: 8},
      ],
      from: 0,
      to: 8,
    }
  );
});

it("completes 'from <table>'", () => {
  assert.deepEqual(
    completions("fr| rest", 2, _.identity, ["table1", "table2"], []),
    {
      list: [
        {text: "FROM table1;", from: 0, to: 2},
        {text: "FROM table2;", from: 0, to: 2},
        {text: "FROM", from: 0, to: 2},
      ],
      from: 0,
      to: 2,
    }
  );
});

it("completes table names", () => {
  assert.deepEqual(
    completions("show tab| rest", 8, _.identity, ["table1", "table2"], []),
    {
      list: [
        {text: "SHOW TABLES;", from: 0, to: 8},
        {text: "table1", from: 5, to: 8},
        {text: "table2", from: 5, to: 8},
      ],
      from: 0,
      to: 8,
    }
  );
});

it("completes column names", () => {
  assert.deepEqual(
    completions("col| rest", 3, _.identity, [], ["column1", "column2"]),
    {
      list: [
        {text: "column1", from: 0, to: 3},
        {text: "column2", from: 0, to: 3},
      ],
      from: 0,
      to: 3,
    }
  );
});

it("completes mid-word", () => {
  assert.deepEqual(
    completions("col rest", 2, _.identity, [], ["column1", "column2"]),
    {
      list: [
        {text: "column1", from: 0, to: 3},
        {text: "column2", from: 0, to: 3},
      ],
      from: 0,
      to: 3,
    }
  );
});
