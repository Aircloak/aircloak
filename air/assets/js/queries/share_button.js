// @flow

import React from "react";
import {Modal, Button} from "react-bootstrap";

import type {Result} from "./result";

type Props = {
  result: Result,
}

type State = {
  showModal: boolean
};

export default class ShareButton extends React.Component<Props, State> {
  constructor(props: Props) {
    super(props);

    this.state = {showModal: false};
  }

  privatePermalink(): ?string {
    const {result} = this.props;
    if (result.private_permalink) return `${window.location.origin}${result.private_permalink}`;
  }

  publicPermalink(): ?string {
    const {result} = this.props;
    if (result.public_permalink) return `${window.location.origin}${result.public_permalink}`;
  }

  isEnabled() {
    const {result} = this.props;
    return result.private_permalink || result.public_permalink;
  }

  render() {
    const {showModal} = this.state;
    if (this.isEnabled()) {
      return (
        <span>
          <button type="button" className="btn btn-default btn-xs" onClick={() => this.setState({showModal: true})}>Share</button>

          <Modal show={showModal} onHide={() => this.setState({showModal: false})}>
            <Modal.Header>
              <Modal.Title>Share</Modal.Title>
            </Modal.Header>

            <Modal.Body>
              <div className="form-group">
                <label>Public link</label>
                <p className="help-block">Anyone with this link will be able to view the query and its results.</p>
                <input className="form-control" readOnly value={this.publicPermalink()} />
              </div>

              <div className="form-group">
                <label>Private link</label>
                <p className="help-block">
                  This link requires logging in with an Insights Air account that is allowed to access this data source.
                </p>
                <input className="form-control" readOnly value={this.privatePermalink()} />
              </div>
            </Modal.Body>

            <Modal.Footer>
              <Button onClick={() => this.setState({showModal: false})}>Close</Button>
            </Modal.Footer>
          </Modal>
        </span>
      );
    } else {
      return null;
    }
  }
}
