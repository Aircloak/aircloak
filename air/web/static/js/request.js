// @flow

import $ from "jquery";

export type Authentication = {CSRFToken: string};

const headers = (csrfToken) => ({
  "X-CSRF-TOKEN": csrfToken,
  "Content-Type": "application/json",
});

export const cancel = (queryId: string, csrfToken: string) =>
  $.ajax("/queries/cancel", {
    method: "POST",
    headers: headers(csrfToken),
    data: JSON.stringify({id: queryId}),
  });

export const startQuery = (queryData, csrfToken, callbacks) => {
  $.ajax("/queries", {
    method: "POST",
    headers: headers(csrfToken),
    data: queryData,
    success: callbacks.success,
    error: callbacks.error,
  });
};

export const loadHistory = (dataSourceId, before, csrfToken, callbacks) => {
  $.ajax(`/queries/load_history/${dataSourceId}?before=${before}`, {
    method: "GET",
    headers: headers(csrfToken),
    success: callbacks.success,
    error: callbacks.error,
  });
};
