// @flow

import type { Node } from "react";
import React, { useState } from "react";

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

export default ({
  reactExports,
  numberFormat,
  debugModeEnabled,
  authentication,
}: Props): Node => {
  const [results, setResults] = useState([]);
  reactExports.setResults = setResults;

  const deleteResult = (id: string) => {
    if (window.confirm("Do you want to permanently delete this query?")) {
      deleteQueryResult(id, authentication);
      setResults((queries) => queries.filter((r) => r.id !== id));
    }
  };

  const updateNote = (id: string, note: string | null) => {
    setQueryNote(id, note, authentication);
    setResults((queries) =>
      queries.map((r: any) => (r.id !== id ? r : { ...r, note }))
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
