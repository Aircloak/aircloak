// @flow

import _ from "lodash";

const KEYWORDS = [
  "SELECT", "FROM",
  "COUNT(*)", "COUNT(columnName)", "COUNT(distinct columnName)",
  "COUNT_NOISE(*)", "COUNT_NOISE(columnName)", "COUNT_NOISE(distinct columnName)",
  "SUM(numericalColumn)", "SUM_NOISE(numericalColumn)",
  "MIN(numericalColumn)", "MAX(numericalColumn)",
  "AVG(numericalColumn)", "AVG_NOISE(numericalColumn)",
  "STDDEV(numericalColumn)", "STDDEV_NOISE(numericalColumn)",
  "MEDIAN(numericalColumn)",
  "EXTRACT(timeUnit FROM dateTimeColumn",
  "EXTRACT_MATCH(stringColumn, 'regex')", "EXTRACT_MATCHES(stringColumn, 'regex')",
  "YEAR(dateColumn)", "QUARTER(dateColumn)", "MONTH(dateColumn)", "WEEKDAY(dateColumn)", "DAY(dateColumn)",
  "HOUR(dateColumn)", "MINUTE(dateColumn)", "SECOND(dateColumn)",
  "ABS(numericalColumn)", "CEIL(numericalColumn)", "CEILING(numericalColumn)",
  "ROUND(numericalColumn)", "SQRT(numericalColumn)", "TRUNC(numericalColumn)",
  "DIV(numericalColumn, numericalConstant)", "FLOOR(numericalColumn)",
  "MOD(numericalColumn, numericalConstant)", "POW(numericalColumn, numericalConstant)",
  "BTRIM(stringColumn)", "BTRIM(stringColumn, 'stringConstant')",
  "LTRIM(stringColumn)", "LTRIM(stringColumn, 'stringConstant')",
  "RTRIM(stringColumn)", "RTRIM(stringColumn, 'stringConstant')",
  "CONCAT(stringColumn, stringColumnOrConstant)",
  "LEFT(stringColumn, integerConstant)", "RIGHT(stringColumn, integerConstant)",
  "LENGTH(stringColumn)",
  "HEX(stringColumn)",
  "DATE_TRUNC('year', dateTimeColumn)", "DATE_TRUNC('quarter', dateTimeColumn)",
  "DATE_TRUNC('month', dateTimeColumn)", "DATE_TRUNC('day', dateTimeColumn)",
  "DATE_TRUNC('hour', dateTimeColumn)", "DATE_TRUNC('minute', dateTimeColumn)",
  "DATE_TRUNC('second', dateTimeColumn)",
  "LOWER(stringColumn)", "LCASE(stringColumn)", "UPPER(stringColumn)", "UCASE(stringColumn)",
  "SUBSTRING(stringColumn FROM integerConstant)",
  "SUBSTRING(stringColumn FROM integerConstant FOR integerConstant)",
  "SUBSTRING(stringColumn FOR integerConstant)",
  "CAST(column AS type)", "CAST(column, type)",
  "BUCKET(numericalColumn by numericalConstant)",
  "BUCKET(numericalColumn by numericalConstant align LOWER)",
  "BUCKET(numericalColumn by numericalConstant align MIDDLE)",
  "BUCKET(numericalColumn by numericalConstant align UPPER)",
  "SHOW TABLES",
  "LEFT JOIN", "LEFT INNER JOIN", "RIGHT INNER JOIN",
  "OUTER JOIN", "LEFT OUTER JOIN", "RIGHT OUTER JOIN",
  "WHERE", "AND",
  "GROUP BY", "ORDER BY",
  "ASC", "DESC", "NOT",
  "IS NULL", "IS NOT NULL",
  "LIKE ''", "ILIKE ''", "NOT LIKE ''", "NOT ILIKE ''",
  "IN ()", "NOT IN ()",
];

const longestFirst = (candidate) => -candidate.text.length;

const wordCharRegex = /(\w|\.)/;

const wordEnd = (string, start) => {
  let end = start;

  while (end < string.length && wordCharRegex.test(string.charAt(end))) {
    end++;
  }

  return end;
};

const escapeWord = (word) => word.replace(/[\-\[\]\/\{\}\(\)\*\+\?\.\\\^\$\|]/g, "\\$&");

export default function completionList(
  curLine: string,
  curPos: number,
  posBuilder: (x: number) => any,
  tableNames: string[],
  columnNames: string[],
  statement: string,
) {
  const end = wordEnd(curLine, curPos);

  // We want to construct the longest possible match using the previous
  // SQL words the analyst has written. For example, if the analyst has
  // already written "LEFT INNER J", then we want to take this into account,
  // and only favour "LEFT INNER JOIN" as a match clause.
  // The current approach taken doesn't work well across lines, as we (if
  // we take the full document into account), lose the location info of
  // where the match starts, when prepping and creating the RegExp match string.
  // This could be worked around, but it seems only for marginal gains.
  const rawCodeWords = _.split(curLine.slice(0, end), /[\s\(]/);
  const codeWords = _.map(rawCodeWords, escapeWord);
  const potentialMatchSequences = [];
  for (let i = codeWords.length; i >= 0; i--) {
    const wordsToUse = [];
    for (let j = i; j < codeWords.length; j++) {
      wordsToUse.push(codeWords[j]);
    }
    if (wordsToUse.length > 0) {
      const potentialMatchSequence = `(${wordsToUse.join(" ")})?`;
      potentialMatchSequences.push(potentialMatchSequence);
    }
  }
  const finalClause = _.chain(potentialMatchSequences).
    reverse().
    join("").
    value();

  const keywordsFromStatement = _.chain(statement).
    split(/[\s\(\),]/).
    reject((word) => word.length < 3).
    reject((word) => _.last(rawCodeWords) === word).
    value();

  const matcher = new RegExp(finalClause, "i");

  const showColumnsFromTables =
    _.map(tableNames, tableName => `SHOW COLUMNS FROM ${tableName}`);

  const fromWithTables =
    _.map(tableNames, tableName => `FROM ${tableName}`);

  const list = _.chain(KEYWORDS).
    concat(tableNames).
    concat(showColumnsFromTables).
    concat(fromWithTables).
    concat(columnNames).
    concat(keywordsFromStatement).
    map((candidate) => {
      const bestMatch = candidate.match(matcher).shift();
      if (bestMatch === "") {
        return null;
      } else {
        return {
          text: candidate,
          from: posBuilder(end - bestMatch.length),
          to: posBuilder(end),
        };
      }
    }).
    reject((candidate) => candidate === null).
    uniqBy((candidate) => _.upperCase(candidate.text)).
    sortBy(longestFirst).
    value();

  if (list.length > 0) {
    // CodeMirror expects there being a global from/to pair, despite these being
    // declared as part of the suggestion itself. We take this from the first
    // provided suggestion for lack of better alternatives.
    return {list, from: list[0].from, to: list[0].to};
  } else {
    return {list, from: posBuilder(0), to: posBuilder(0)};
  }
}
