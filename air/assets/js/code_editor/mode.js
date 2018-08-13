// @flow

import _ from "lodash";
import CodeMirror from "codemirror";

// the function completion keywords are automatically generated during compilation
/* eslint-disable */
import aircloakFunctionCompletions from "./function_completion_keywords.json";
/* eslint-enable */

const createModeDefinition = () => {
  const set = (str) => {
    const obj = {};
    const words = str.split(" ");
    for (let i = 0; i < words.length; ++i) obj[words[i]] = true;
    return obj;
  };

  const basicSqlKeywords = "and as asc between by count desc distinct from group " +
    "having in is join like not on or order select where limit ";

  const aircloakFunctionsList = _.chain(aircloakFunctionCompletions).
    keys().
    join(" ").
    value();

  return {
    name: "sql",
    client: set("source"),
    keywords: set(basicSqlKeywords + aircloakFunctionsList),
    builtin: set("boolean date datetime double int integer real text time"),
    atoms: set("false true null"),
    operatorChars: /^[*+\-%<>!=&|^\/#@?~]/,
    dateSQL: set("date time timestamp"),
    support: set("ODBCdotTable decimallessFloat zerolessFloat binaryNumber hexNumber nCharCast charsetCast"),
  };
};

CodeMirror.defineMIME("text/x-aircloak-sql", createModeDefinition());
