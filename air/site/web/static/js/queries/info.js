import React from "react";

export const Info = (props) => {
  if (props.info && props.info.length > 0) {
    return (
      <div>
        <h4>Info</h4>
        {props.info.map((infoMessage, i) => <p key={i}>{infoMessage}</p>)}
      </div>
    );
  } else {
    return null;
  }
};

Info.propTypes = {
  info: React.PropTypes.array,
};
