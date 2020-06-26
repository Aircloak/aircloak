// @flow

import React from "react";

type Props = {
  initialError: string | null,
};

type State = {
  value: string,
  score: number,
  error: string | null,
};

export default class PasswordField extends React.Component<Props, State> {
  zxcvbn: null | ((string, [string]) => { score: number });
  constructor(props: Props) {
    super(props);

    this.state = { value: "", score: 0, error: props.initialError };

    import("zxcvbn").then((mod) => {
      this.zxcvbn = mod.default;
    });
  }

  updateValue = (e: SyntheticInputEvent<EventTarget>) => {
    const value = e.target.value;
    this.setState({
      value,
      score: this.zxcvbn ? this.zxcvbn(value, ["AirCloak"]).score : 0,
      error: null,
    });
  };

  renderScore = () => {
    const { value, score, error } = this.state;

    if (error) {
      return <div className="invalid-feedback">{error}</div>;
    } else if (value === "") {
      return null;
    } else if (score <= 1) {
      return <div className="invalid-feedback">weak</div>;
    } else if (score <= 2) {
      return (
        <div className="form-text text-warning" style={{ fontSize: "80%" }}>
          medium
        </div>
      );
    } else if (score <= 3) {
      return <div className="valid-feedback">strong</div>;
    } else {
      return <div className="valid-feedback">very strong</div>;
    }
  };

  highlightClass = () => {
    const { value, score, error } = this.state;

    if (error) {
      return "is-invalid";
    } else if (value === "") {
      return "";
    } else if (score <= 1) {
      return "is-invalid";
    } else if (score <= 2) {
      return "border-warning";
    } else {
      return "is-valid";
    }
  };

  render = () => {
    const { value } = this.state;
    return (
      <div>
        <input
          type="password"
          id="user_password"
          name="user[password]"
          className={`form-control ${this.highlightClass()}`}
          autoComplete="new-password"
          value={value}
          onChange={this.updateValue}
        />
        {this.renderScore()}
      </div>
    );
  };
}
