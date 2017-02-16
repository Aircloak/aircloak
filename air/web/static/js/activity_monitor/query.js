// @flow

import React from "react";

import {StateView} from "./state_view";

export type Query = {
  id: string,
  state: string,
  analyst_name: string,
  data_source_name: string,
};

export class QueryView extends React.Component {
  constructor(props: Query) {
    super(props);
  }

  props: Query;

  render() {
    return (
      <tr>
        <td>{this.props.data_source_name}</td>
        <td>{this.props.analyst_name}</td>
        <td><StateView state={this.props.state} /></td>
      </tr>
    );
  }
}
