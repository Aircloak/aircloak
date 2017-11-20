// @flow

import React from "react";
import marked from "marked";

const mdToHtml = (text: string) => ({__html: marked(text)});

export const Info = (props: {info: string[]}) => {
  if (props.info && props.info.length > 0) {
    return (
      <div>
        <h4>Info</h4>
        {props.info.map((infoMessage, i) =>
          <p key={i} dangerouslySetInnerHTML={mdToHtml(infoMessage)} />)
        }
      </div>
    );
  } else {
    return null;
  }
};
