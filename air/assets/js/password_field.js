// @flow

import React from "react";
import zxcvbn from "zxcvbn";

type Props = {};

type State = {
  value: string,
  score: number
};

export default class PasswordField extends React.Component<Props, State> {
  constructor(props: Props) {
    super(props);

    this.state = { value: "", score: 0 };

    this.updateValue = this.updateValue.bind(this);
    this.renderScore = this.renderScore.bind(this);
    this.highlightClass = this.highlightClass.bind(this);
  }

  updateValue = (e: SyntheticInputEvent<EventTarget>) => {
    this.setState({
      value: e.target.value,
      score: zxcvbn(e.target.value).score
    });
  };

  renderScore = () => {
    const { value, score } = this.state;
    if (value === "") {
      return null;
    } else if (score <= 1) {
      return "weak";
    } else if (score <= 2) {
      return "medium";
    } else if (score <= 3) {
      return "strong";
    } else {
      return "very strong";
    }
  };

  highlightClass = () => {
    const { value, score } = this.state;
    if (value === "") {
      return null;
    } else if (score <= 1) {
      return "has-error";
    } else if (score <= 2) {
      return "has-warning";
    } else {
      return "has-success";
    }
  };

  render = () => {
    const { value } = this.state;
    return (
      <div className={this.highlightClass()}>
        <input
          type="password"
          className="form-control"
          value={value}
          onChange={this.updateValue}
        />
        <span className="help-block">{this.renderScore()}</span>
      </div>
    );
  };
}
