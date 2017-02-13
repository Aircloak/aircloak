// @flow

import React from "react";

export const GraphConfigView = (props: {graphInfo: GraphInfoT}) =>
  <form className="form-horizontal">
    <h2>Axes</h2>

    {props.graphInfo.xColumns().map((column, i) => {
      return (
        <div key={i} className="form-group">
          <label className="col-sm-3 control-label">{column}</label>
          <div className="col-sm-9 btn-group" role="group">
            <button type="button" className="btn btn-info">None</button>
            <button type="button" className="btn btn-default">X</button>
            <button type="button" className="btn btn-default" disabled={!props.graphInfo.usableAsY(column)}>Y</button>
          </div>
        </div>
      )
    })}
  </form>
