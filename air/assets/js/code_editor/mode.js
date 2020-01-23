// @flow

import _ from "lodash";
import CodeMirror from "codemirror";

// the function completion keywords are automatically generated during compilation
/* eslint-disable */
import aircloakFunctionCompletions from "./function_completion_keywords.json";
/* eslint-enable */

const createModeDefinition = () => {
  const set = str => {
    const obj = {};
    const words = str.split(" ");
    for (let i = 0; i < words.length; i += 1) obj[words[i]] = true;
    return obj;
  };

  const basicSqlKeywords =
    "align all and as asc between both bucket by cast columns cross cube desc distinct escape" +
    " extract for from full group grouping having ilike in inner is join leading left like limit not nulls offset on" +
    " or order outer right rollup select sets show substring tables trailing trim where";

  const aircloakFunctionsList = _.chain(aircloakFunctionCompletions)
    .keys()
    .join(" ")
    .value();

  return {
    name: "sql",
    client: set("source"),
    keywords: set(basicSqlKeywords + aircloakFunctionsList),
    builtin: set("boolean date datetime integer real text time"),
    atoms: set("false true null"),
    operatorChars: /^[*+\-%<>!=&|^/#@?~]/,
    dateSQL: set("date time timestamp"),
    support: set(
      "ODBCdotTable decimallessFloat zerolessFloat binaryNumber hexNumber nCharCast charsetCast"
    )
  };
};

CodeMirror.defineMIME("text/x-aircloak-sql", createModeDefinition());
