import React from "react";

export const DataSourceSelector = (props) =>
  <select>
    {props.sources.map((source, i) =>
      <option key={i} value={source.id}>{source.display}</option>
    )}
  </select>;

DataSourceSelector.propTypes = {
  sources: React.PropTypes.arrayOf(React.PropTypes.shape({
    id: React.PropTypes.string.isRequired,
    display: React.PropTypes.string.isRequired,
  })).isRequired,
};
