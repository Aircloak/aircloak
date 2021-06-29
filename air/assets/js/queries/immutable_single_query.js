// @flow

import type { Element } from "React";
import React, { useState } from "react";

import Results from "./results";
import PropertiesView from "./properties";
import { deleteQueryResult, setQueryNote } from "../request";
import type { Authentication } from "../authentication_provider";
import type { Result } from "./result";
import type { NumberFormat } from "../number_format";

type Props = {
  result: Result,
  user: { name: string },
  insertedAt: string,
  numberFormat: NumberFormat,
  debugModeEnabled: boolean,
  authentication: Authentication,
  isOwner?: boolean,
};

export default ({
  result,
  user,
  insertedAt,
  numberFormat,
  debugModeEnabled,
  authentication,
  isOwner,
}: Props): Element<"div"> => {
  const deleteResult = isOwner
    ? (id: string) => {
        if (window.confirm("Do you want to permanently delete this query?")) {
          deleteQueryResult(id, authentication);
          window.location.replace("/");
        }
      }
    : undefined;

  const [currentResult, setCurrentResult] = useState(result);
  const updateNote = isOwner
    ? (id: string, note: string | null) => {
        setQueryNote(id, note, authentication);
        setCurrentResult({ ...result, note });
      }
    : undefined;

  return (
    <div>
      <h3>Properties</h3>
      <PropertiesView
        user={user}
        insertedAt={insertedAt}
        dataSource={result.data_source}
        queryState={result.query_state}
      />

      <h3>Query</h3>
      <Results
        numberFormat={numberFormat}
        results={[currentResult]}
        debugModeEnabled={debugModeEnabled}
        authentication={authentication}
        onDeleteClick={deleteResult}
        updateNote={updateNote}
      />
    </div>
  );
};
