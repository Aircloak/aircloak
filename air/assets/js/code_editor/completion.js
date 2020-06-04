// @flow
import sortBy from "lodash/sortBy";
import uniqBy from "lodash/uniqBy";
// the function completion keywords are automatically generated during compilation
/* eslint-disable */
import aircloakFunctionCompletions from "./function_completion_keywords.json";
/* eslint-enable */

const SQL_KEYWORDS = [
  "SELECT",
  "FROM",
  "SHOW TABLES",
  "INNER JOIN",
  "LEFT JOIN",
  "LEFT INNER JOIN",
  "RIGHT INNER JOIN",
  "OUTER JOIN",
  "LEFT OUTER JOIN",
  "RIGHT OUTER JOIN",
  "WHERE",
  "AND",
  "GROUP BY",
  "ORDER BY",
  "ASC",
  "DESC",
  "NOT",
  "IS NULL",
  "IS NOT NULL",
  "LIKE ''",
  "ILIKE ''",
  "NOT LIKE ''",
  "NOT ILIKE ''",
  "IN ()",
  "NOT IN ()",
];

const longestFirst = (candidate) => -candidate.text.length;

const wordCharRegex = /(\w|\.)/;

const wordEnd = (string, start) => {
  let end = start;

  while (end < string.length && wordCharRegex.test(string.charAt(end))) {
    end += 1;
  }

  return end;
};

const escapeWord = (word) => word.replace(/[-[\]/{}()*+?.\\^$|]/g, "\\$&");

export default function completionList(
  curLine: string,
  curPos: number,
  posBuilder: (x: number) => any,
  tableNames: string[],
  columnNames: string[],
  statement: string
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
  const rawCodeWords = curLine.slice(0, end).split(/[\s(]/);
  const codeWords = rawCodeWords.map(escapeWord);
  const potentialMatchSequences = [];
  for (let i = codeWords.length; i >= 0; i -= 1) {
    const wordsToUse = [];
    for (let j = i; j < codeWords.length; j += 1) {
      wordsToUse.push(codeWords[j]);
    }
    if (wordsToUse.length > 0) {
      const potentialMatchSequence = `(${wordsToUse.join(" ")})?`;
      potentialMatchSequences.push(potentialMatchSequence);
    }
  }
  const finalClause = Array.from(potentialMatchSequences).reverse().join("");

  const keywordsFromStatement: string[] = statement
    .split(/[\s(),]/)
    .filter((word) => word.length >= 3)
    .filter((word) => rawCodeWords[rawCodeWords.length - 1] !== word);

  const matcher = new RegExp(finalClause, "i");

  const showColumnsFromTables: string[] = tableNames.map(
    (tableName) => `SHOW COLUMNS FROM ${tableName}`
  );

  const fromWithTables = tableNames.map((tableName) => `FROM ${tableName}`);

  const aircloakSQLFunctions: string[] = (Object.values(
    aircloakFunctionCompletions
  ).flat(): any);

  const listBase = SQL_KEYWORDS.concat(
    aircloakSQLFunctions,
    tableNames,
    showColumnsFromTables,
    fromWithTables,
    columnNames,
    keywordsFromStatement
  )
    .map((candidate: string) => {
      const matches = candidate.match(matcher);
      if (!matches) return null;
      const bestMatch = matches.shift();
      if (bestMatch === "") {
        return null;
      } else {
        return {
          text: candidate,
          from: posBuilder(end - bestMatch.length),
          to: posBuilder(end),
        };
      }
    })
    .filter((candidate) => candidate !== null);

  const list = sortBy(
    uniqBy(listBase, (candidate) => candidate.text.toUpperCase()),
    longestFirst
  );

  if (list.length > 0) {
    // CodeMirror expects there being a global from/to pair, despite these being
    // declared as part of the suggestion itself. We take this from the first
    // provided suggestion for lack of better alternatives.
    return { list, from: list[0].from, to: list[0].to };
  } else {
    return { list, from: posBuilder(0), to: posBuilder(0) };
  }
}
