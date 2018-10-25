// @flow

import React from "react";
import {Modal, Button} from "react-bootstrap";

import type {Result} from "./result";

type Props = {
  result: Result,
}

export class ShareButton extends React.Component {
  constructor(props: Props) {
    super(props);

    this.state = {showModal: false};
  }

  state: {showModal: boolean}

  privatePermalink() {
    return `${window.location.origin}${this.props.result.private_permalink}`;
  }

  publicPermalink() {
    return `${window.location.origin}${this.props.result.public_permalink}`;
  }

  isEnabled() {
    return this.props.result.private_permalink || this.props.result.public_permalink;
  }

  render() {
    if (this.isEnabled()) {
      return (
        <span>
          <a className="btn btn-default btn-xs" onClick={() => this.setState({showModal: true})}>Share</a>

          <Modal show={this.state.showModal} onHide={() => this.setState({showModal: false})}>
            <Modal.Header>
              <Modal.Title>Share</Modal.Title>
            </Modal.Header>

            <Modal.Body>
              <div className="form-group">
                <label>Public link</label>
                <p className="help-block">Anyone with this link will be able to view the query and its results.</p>
                <input className="form-control" readOnly="true" value={this.publicPermalink()} />
              </div>

              <div className="form-group">
                <label>Private link</label>
                <p className="help-block">
                  This link requires logging in with an Insights Air account that is allowed to access this data source.
                </p>
                <input className="form-control" readOnly="true" value={this.privatePermalink()} />
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
