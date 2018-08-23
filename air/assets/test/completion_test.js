import _ from "lodash";
import assert from "assert";

import completions from "code_editor/completion";

it("completes keywords", () => {
  assert.deepEqual(
    completions("thing cou| rest", 9, _.identity, [], [], ""),
    {
      list: [
        {text: "count_noise(<any>)", from: 6, to: 9},
        {text: "count(<any>)", from: 6, to: 9},
      ],
      from: 6,
      to: 9,
    }
  );
});

it("completes expressions", () => {
  assert.deepEqual(
    completions("show tab| rest", 8, _.identity, [], [], ""),
    {
      list: [{text: "SHOW TABLES", from: 0, to: 8}],
      from: 0,
      to: 8,
    }
  );
});

it("completes 'show columns from <table>'", () => {
  assert.deepEqual(
    completions("show col| rest", 8, _.identity, ["table1", "table2"], ["a column"], ""),
    {
      list: [
        {text: "SHOW COLUMNS FROM table1", from: 0, to: 8},
        {text: "SHOW COLUMNS FROM table2", from: 0, to: 8},
      ],
      from: 0,
      to: 8,
    }
  );
});

it("completes 'from <table>'", () => {
  assert.deepEqual(
    completions("fr| rest", 2, _.identity, ["table1", "table2"], [], ""),
    {
      list: [
        {text: "FROM table1", from: 0, to: 2},
        {text: "FROM table2", from: 0, to: 2},
        {text: "FROM", from: 0, to: 2},
      ],
      from: 0,
      to: 2,
    }
  );
});

it("completes table names", () => {
  assert.deepEqual(
    completions("show tab| rest", 8, _.identity, ["table1", "table2"], [], ""),
    {
      list: [
        {text: "SHOW TABLES", from: 0, to: 8},
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
    completions("col| rest", 3, _.identity, [], ["column1", "column2"], ""),
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

it("deduplicates suggestions", () => {
  assert.deepEqual(
    completions("col", 3, _.identity, [], ["column", "column"], ""),
    {
      list: [{text: "column", from: 0, to: 3}],
      from: 0,
      to: 3,
    }
  );
});

it("completes mid-word", () => {
  assert.deepEqual(
    completions("col rest", 2, _.identity, [], ["column1", "column2"], ""),
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

it("escapes escape sequences", () => {
  assert.deepEqual(
    completions("\\", 1, _.identity, [], [], ""),
    {
      list: [],
      from: 0,
      to: 0,
    }
  );
});

it("completes after parens", () => {
  assert.deepEqual(
    completions("count(coun", 10, _.identity, [], [], ""),
    {
      list: [
        {text: "count_noise(<any>)", from: 6, to: 10},
        {text: "count(<any>)", from: 6, to: 10},
      ],
      from: 6,
      to: 10,
    }
  );
});

it("completes based on what else has been written in query", () => {
  assert.deepEqual(
    completions("alias", 5, _.identity, [], [], "SELECT column as aliased_name"),
    {
      list: [{text: "aliased_name", from: 0, to: 5}],
      from: 0,
      to: 5,
    }
  );
});

it("completes based on what else has been written in query but excludes the current word", () => {
  assert.deepEqual(
    completions("alias", 5, _.identity, [], [], "SELECT alias as aliased_name"),
    {
      list: [{text: "aliased_name", from: 0, to: 5}],
      from: 0,
      to: 5,
    }
  );
});

it("completions based on written query do not contain trailing comman", () => {
  assert.deepEqual(
    completions("alias", 5, _.identity, [], [], "SELECT alias as aliased_name, other FROM"),
    {
      list: [{text: "aliased_name", from: 0, to: 5}],
      from: 0,
      to: 5,
    }
  );
});
