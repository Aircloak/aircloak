// @flow
import React from "react";
import type { Column } from "./columns";
import fuzzysort from "fuzzysort";

export type Filter = {
  query: string,
  types: any,
  fuzzy: boolean,
};

export const emptyFilter = (): Filter => ({
  query: "",
  fuzzy: true,
  types: {
    text: true,
    date: true,
    real: true,
    integer: true,
    boolean: true,
    user_id: true,
    foreign: true,
    other: true,
  },
});

const regularTypes = ["text", "date", "real", "integer", "boolean"];

const normalizeType = (column: Column): string => {
  if (column.key_type === "user_id") return "user_id";
  if (column.key_type) return "foreign";
  if (column.type === "datetime") return "date"; // these have the same icon now, so we treat them as the same
  if (regularTypes.includes(column.type)) return column.type;
  return "other";
};

/**
 * Returns true if the filter is "neutral", i.e. will match all results.
 * If so, we can skip all filtering work and just return all results.
 */
export const isEmptyFilter = ({ query, types }: Filter): boolean =>
  query.trim() === "" && Object.values(types).every((a) => a);

/**
 * A combination of Array.prototype.filter and Array.prototype.map. Will remove any `null` values returned.
 */
function filterMap<T, U>(arr: Array<T>, fn: (T) => U | null): Array<U> {
  const result = [];
  for (let item of arr) {
    const res = fn(item);
    if (res !== null) {
      result.push(res);
    }
  }
  return result;
}

/**
 * Returns an array with increasing integers from begin to end.
 * Assumes begin <= end.
 */
const range = (begin: number, end: number): number[] =>
  Array.from(new Array(end - begin)).map((_, i) => begin + i);

/**
 * Returns all permutations of length `n`.
 *
 *     permutations(3)
 *     // [
 *     //   [0, 1, 2],
 *     //   [0, 2, 1],
 *     //   [1, 0, 2],
 *     //   [1, 2, 0],
 *     //   [2, 0, 1],
 *     //   [2, 1, 0],
 *     // ]
 */
const permutations = (n: number): number[][] => {
  var inputArr = range(0, n);
  var results = [];

  function permute(arr, memo) {
    var cur;

    for (var i = 0; i < arr.length; i++) {
      cur = arr.splice(i, 1);
      if (arr.length === 0) {
        results.push(memo.concat(cur));
      }
      permute(arr.slice(), memo.concat(cur));
      arr.splice(i, 0, cur[0]);
    }

    return results;
  }

  return permute(inputArr, []);
};

/**
 * Finds the best assignment of variables in array `as` to variables in array `bs` according to `costFn`.
 *
 * Returns:
 *    - the total cost of the best solution
 *    - additional metadata returned by the `costFn` for the optimal solution.
 */
function linearBalancedAssignment<T, M>(
  as: Array<T>,
  bs: Array<T>,
  costFn: (T, T) => [number, M]
): [number, Array<M>] {
  // This is a naive algorithm with O(n!) complexity.
  // This can be improved to O(n^3) complexity at the cost of much more complex code.
  // However, we presume that:
  //  1. `as` will be fixed to at 3 (table, column, type)
  //  2. `bs` will not be fixed, but should be small (the user won't type too many words)
  //  3. we can limit `bs` to be the same length as `as`, as there is no point in having more
  // As such, we don't need to care about the runtime complexity so much.

  bs = bs.slice(0, as.length);
  const costMatrix = as.map((a) => bs.map((b) => costFn(a, b)));
  return permutations(as.length).reduce(
    ([max, metadata], assignment) => {
      let score = 0;
      for (let i = 0; i < bs.length; i++) {
        score += costMatrix[assignment[i]][i][0];
      }
      if (score > max) {
        let newMetadata = new Array(as.length);
        for (let i = 0; i < bs.length; i++) {
          newMetadata[assignment[i]] = costMatrix[assignment[i]][i][1];
        }
        return [score, newMetadata];
      } else {
        return [max, metadata];
      }
    },
    [-Infinity, []]
  );
}

/**
 * Cost function for fuzzy search. Returns [score, indices of matched substrings].
 */
const fuzzy = (val: any, q: any): [number, number[]] => {
  const res = fuzzysort.single(q, val, { allowTypo: false });
  if (res) {
    return [res.score, res.indexes];
  }
  return [-Infinity, []];
};

/**
 * Cost function for exact search. Returns [score, indices of matched substring].
 */
const exact = (val: string, q: string): [number, number[]] => {
  if (val.includes(q)) {
    const begin = val.indexOf(q);
    return [val === q ? 1.1 : 1, range(begin, begin + q.length)];
  }

  return [-Infinity, []];
};

const prepareSolver = (
  filter: Filter,
  table: string
): ((Column) => [number, number[][]]) => {
  let queries = filter.query.split(/\s+/).filter((q) => q.trim() !== "");
  if (filter.fuzzy) {
    queries = queries.map((q) => fuzzysort.prepareSearch(q));
    const tableIndex = fuzzysort.prepare(table);
    return (column) => {
      if (!column._indexEntry) {
        Object.assign(column, {
          _indexEntry: fuzzysort.prepare(column.name),
        });
      }
      return linearBalancedAssignment(
        ([column._indexEntry, tableIndex, column.type]: any[]),
        (queries: any[]),
        fuzzy
      );
    };
  } else {
    return (column) =>
      linearBalancedAssignment(
        ([column.name, table, column.type]: string[]),
        (queries: string[]),
        exact
      );
  }
};

export const filterColumns = (
  table: string,
  columns: Column[],
  filter: Filter
): Column[] => {
  if (isEmptyFilter(filter)) return columns;

  const solver = prepareSolver(filter, table);

  return filterMap(columns, (column) => {
    if (!filter.types[normalizeType(column)]) {
      return null;
    }

    if (filter.query.trim() === "") return column;

    const [total, [columnIndices, tableIndices, typeIndices]] = solver(column);

    return total > -10000
      ? Object.assign({}, column, {
          _matchIndexes: {
            name: columnIndices,
            table: tableIndices,
            type: typeIndices,
          },
        })
      : null;
  });
};

type Props = {
  table: string,
  column: Column,
  field: string,
};

export const Higlighted = ({ table, column, field }: Props) => {
  const text = field === "table" ? table : column[field];
  if (!column._matchIndexes) return text;

  const indexes = column._matchIndexes[field];
  if (!indexes || indexes.length === 0) return text;
  let result = [];
  let j = 0;
  let k = indexes[0];
  indexes.forEach((index, i) => {
    if (j !== index) {
      result.push(
        <React.Fragment key={i}>{text.slice(j, index)}</React.Fragment>
      );
    }
    j = index + 1;
    if (i + 1 === indexes.length) {
      result.push(
        <mark className="highlight" key={`highlight-${i}`}>
          {text.slice(k, index + 1)}
        </mark>
      );
      result.push(
        <React.Fragment key={i + 1}>{text.slice(index + 1)}</React.Fragment>
      );
    } else if (index + 1 !== indexes[i + 1]) {
      result.push(
        <mark className="highlight" key={`highlight-${i}`}>
          {text.slice(k, index + 1)}
        </mark>
      );
      k = indexes[i + 1];
    }
  });
  return <>{result}</>;
};

export const needsHighlighting = (column: Column) =>
  column._matchIndexes && column._matchIndexes.type;
