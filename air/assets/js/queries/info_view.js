// @flow
/* eslint-disable react/no-danger */

import React from "react";
import pagedown from "pagedown";

const mdToHtml = (text: string) => ({__html: pagedown.getSanitizingConverter().makeHtml(text)});

export default ({info}: {info: string[]}) => {
  if (info && info.length > 0) {
    return (
      <div>
        <h4>Info</h4>
        {info.map((infoMessage) => <p dangerouslySetInnerHTML={mdToHtml(infoMessage)} />)}
      </div>
    );
  } else {
    return null;
  }
};
