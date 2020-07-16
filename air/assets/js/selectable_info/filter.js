// @flow
import React from "react";
import type { Column } from "./columns";
import fuzzysort from "fuzzysort";

export type Result = Column & { _indexEntry?: any, _matchIndexes?: number[] };

export const filterColumns = (
  table: string,
  columns: any,
  query: string,
  opts: { limit?: number } = {}
): Result[] => {
  if (query === "") return columns;
  columns.forEach((column) => {
    // this mutation is a performance hack
    if (!column._indexEntry) {
      Object.assign(column, {
        _indexEntry: fuzzysort.prepare(
          `${table}${column.name}${column.type}${column.key_type || ""}`
        ),
      });
    }
  });
  return fuzzysort
    .go(query, columns, {
      threshold: -99999,
      limit: opts.limit != null ? opts.limit : 500,
      allowTypo: false,
      key: "_indexEntry",
    })
    .map(({ obj, indexes }) => ({ _matchIndexes: indexes, ...obj }));
};

type Props = {
  table: string,
  column: Result,
  field: string,
};

export const Higlighted = ({ table, column, field }: Props) => {
  const text = field === "table" ? table : column[field];
  if (!column._matchIndexes) return text;

  let minIndex = 0,
    maxIndex = 0;
  if (field === "table") {
    maxIndex = table.length;
  } else {
    minIndex += table.length;
    if (field === "name") {
      maxIndex = minIndex + column.name.length;
    } else {
      minIndex += column.name.length;
      if (field === "type") {
        maxIndex = minIndex + column.type.length;
      } else {
        minIndex += column.type.length;
        maxIndex = minIndex + (column.key_type || "").length;
      }
    }
  }
  const indexes = [];
  for (let index of column._matchIndexes) {
    if (minIndex <= index && index < maxIndex) {
      indexes.push(index - minIndex);
    }
  }
  if (indexes.length === 0) return text;
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
