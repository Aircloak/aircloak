// @flow

import $ from "jquery";

export type Authentication = {CSRFToken: string};

export const cancel = (queryId: string, csrfToken: string) =>
  $.ajax("/queries/cancel", {
    method: "POST",
    headers: {
      "X-CSRF-TOKEN": csrfToken,
      "Content-Type": "application/json",
    },
    data: JSON.stringify({id: queryId}),
  });
