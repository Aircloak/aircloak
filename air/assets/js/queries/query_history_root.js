// @flow

import type { Node } from "react";
import React, { useState } from "react";

import type { Result } from "./result";
import Results from "./results";
import { deleteQueryResult, setQueryNote } from "../request";
import type { Authentication } from "../authentication_provider";
import type { NumberFormat } from "../number_format";

type Props = {
  reactExports: { [string]: any },
  numberFormat: NumberFormat,
  debugModeEnabled: boolean,
  authentication: Authentication,
};

const resultsPending: Result[] = (null: any);

export default ({
  reactExports,
  numberFormat,
  debugModeEnabled,
  authentication,
}: Props): Node => {
  const [results, setResults] = useState(resultsPending);
  reactExports.setResults = setResults;

  if (results === resultsPending) {
    return null;
  } else if (results.length === 0) {
    return <p>There are no queries matching your search criteria.</p>;
  }

  const deleteResult = (id: string) => {
    if (window.confirm("Do you want to permanently delete this query?")) {
      deleteQueryResult(id, authentication);
      setResults((results: Result[]) => results.filter((r) => r.id !== id));
    }
  };

  const updateNote = (id: string, note: string | null) => {
    setQueryNote(id, note, authentication);
    setResults((results: Result[]) =>
      results.map((r: any) => (r.id !== id ? r : { ...r, note }))
    );
  };

  return (
    <Results
      numberFormat={numberFormat}
      results={results}
      debugModeEnabled={debugModeEnabled}
      authentication={authentication}
      onDeleteClick={deleteResult}
      updateNote={updateNote}
      hideCancelButton={true}
    />
  );
};
