// @flow

import $ from "jquery";

export type Authentication = {CSRFToken: string};

type Callback = (result: any) => void;
type Callbacks = {success?: Callback, error?: Callback};

export type QueryData = {
  statement: string,
  data_source_id: number,
  session_id: string,
};

const headers = ({CSRFToken}) => ({
  "X-CSRF-TOKEN": CSRFToken,
  "Content-Type": "application/json",
});

export const cancel = (queryId: string, authentication: Authentication) =>
  $.ajax("/queries/cancel", {
    method: "POST",
    headers: headers(authentication),
    data: JSON.stringify({id: queryId}),
  });

export const startQuery = (queryData: QueryData, authentication: Authentication, callbacks: Callbacks) => {
  $.ajax("/queries", {
    method: "POST",
    headers: headers(authentication),
    data: queryData,
    success: callbacks.success,
    error: callbacks.error,
  });
};

export const loadHistory = (
  dataSourceId: number,
  before: string,
  authentication: Authentication,
  callbacks: Callbacks
) => {
  $.ajax(`/queries/load_history/${dataSourceId}?before=${before}`, {
    method: "GET",
    headers: headers(authentication),
    success: callbacks.success,
    error: callbacks.error,
  });
};
