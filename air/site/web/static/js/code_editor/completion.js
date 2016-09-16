import _ from "lodash";

const KEYWORDS = [
  "SELECT", "FROM",
  "COUNT(*)", "SUM()", "MIN()", "MAX()", "AVG()", "STDDEV()", "MEDIAN()",
  "SHOW TABLES;",
  "LEFT JOIN", "LEFT INNER JOIN", "RIGHT INNER JOIN",
  "OUTER JOIN", "FULL OUTER JOIN", "LEFT OUTER JOIN", "RIGHT OUTER JOIN",
  "WHERE", "AND",
  "GROUP BY", "ORDER BY",
  "ASC", "DESC", "NOT",
  "IS NULL", "IS NOT NULL",
  "LIKE ''", "ILIKE ''", "NOT LIKE ''", "NOT ILIKE ''",
  "IN ()", "NOT IN ()",
];

export default function completionList(cm, editor, tableNames, columnNames) {
  const cur = cm.getCursor();
  const curLine = cm.getLine(cur.line);
  const regex = /(\w|\.)/;
  let start = cur.ch;
  let end = start;

  // find start and end of the word
  while (end < curLine.length && regex.test(curLine.charAt(end))) {
    end++;
  }
  while (start > 0 && regex.test(curLine.charAt(start - 1))) {
    start--;
  }

  // We want to construct the longest possible match using the previous
  // SQL words the analyst has written. For example, if the analyst has
  // already written "LEFT INNER J", then we want to take this into account,
  // and only favour "LEFT INNER JOIN" as a match clause.
  // The current approach taken doesn't work well across lines, as we (if
  // we take the full document into account), lose the location info of
  // where the match starts, when prepping and creating the RegExp match string.
  // This could be worked around, but it seems only for marginal gains.
  const codeWords = curLine.slice(0, end).split(/\s/);
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

  const matcher = new RegExp(finalClause, "i");

  const sortOrder = (candidate) =>
    // We favour the longest matches over the more specific ones
    candidate.text.length * -1;

  const showColumnsFromTables =
    _.map(tableNames, tableName => `SHOW COLUMNS FROM ${tableName};`);

  const fromWithTables =
    _.map(tableNames, tableName => `FROM ${tableName};`);

  const list = _.chain(KEYWORDS).
  concat(tableNames).
  concat(showColumnsFromTables).
  concat(fromWithTables).
  concat(columnNames).
  map((candidate) => {
    const bestMatch = candidate.match(matcher).shift();
    if (bestMatch === "") {
      return null;
    } else {
      return {
        text: candidate,
        /* eslint-disable new-cap */
        from: editor.Pos(cur.line, end - bestMatch.length),
        to: editor.Pos(cur.line, end),
        /* eslint-enable new-cap */
      };
    }
  }).
  reject((candidate) => candidate === null).
  sortBy((item) => sortOrder(item)).
  value();

  return {
    list,
    /* eslint-disable new-cap */
    from: editor.Pos(cur.line, start),
    to: editor.Pos(cur.line, end),
    /* eslint-enable new-cap */
  };
}
