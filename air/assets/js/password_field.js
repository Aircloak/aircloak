// @flow

import React from "react";
import zxcvbn from "zxcvbn";

export default class PasswordField extends React.Component {
  constructor(props) {
    super(props);

    this.state = {value: ""};

    this.updateValue = this.updateValue.bind(this);
    this.renderScore = this.renderScore.bind(this);
    this.highlightClass = this.highlightClass.bind(this);
  }

  updateValue(event) {
    this.setState({value: event.target.value, score: zxcvbn(event.target.value).score});
  }

  renderScore() {
    if (this.state.value == "") {
      return null;
    } else if (this.state.score <= 1) {
      return "weak";
    } else if (this.state.score <= 2) {
      return "medium";
    } else if (this.state.score <= 3) {
      return "strong";
    } else {
      return "very strong";
    }
  }

  highlightClass() {
    if (this.state.value == "") {
      return null;
    } else if (this.state.score <= 1) {
      return "has-error";
    } else if (this.state.score <= 2) {
      return "has-warning";
    } else {
      return "has-success";
    }
  }

  render() {
    return <div className={this.highlightClass()}>
        <input type="password" value={this.state.value} onChange={this.updateValue} {...this.props}></input>
        <span className="help-block">{this.renderScore()}</span>
      </div>;
  }
}
