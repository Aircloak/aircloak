// @flow

import React from "react";
import Modal from "react-bootstrap/Modal";
import Button from "react-bootstrap/Button";

import type { Result } from "./result";

import copyToClipboard from "../copy_to_clipboard";

type Props = {
  result: Result,
};

type State = {
  showModal: boolean,
};

export default class ShareButton extends React.Component<Props, State> {
  constructor(props: Props) {
    super(props);

    this.state = { showModal: false };
  }

  privatePermalink(): ?string {
    const { result } = this.props;
    if (result.private_permalink) {
      return `${window.location.origin}${result.private_permalink}`;
    } else {
      return null;
    }
  }

  publicPermalink(): ?string {
    const { result } = this.props;
    if (result.public_permalink) {
      return `${window.location.origin}${result.public_permalink}`;
    } else {
      return null;
    }
  }

  isEnabled() {
    const { result } = this.props;
    return result.private_permalink || result.public_permalink;
  }

  selectAll(event: any) {
    event.target.select();
  }

  render() {
    const { showModal } = this.state;
    if (this.isEnabled()) {
      return (
        <>
          <button
            type="button"
            className="btn btn-outline-secondary btn-sm"
            onClick={() => this.setState({ showModal: true })}
          >
            <i className="fas fa-share"></i> Share
          </button>

          <Modal
            show={showModal}
            onHide={() => this.setState({ showModal: false })}
            size="lg"
            centered
          >
            <Modal.Header closeButton>
              <Modal.Title>Share</Modal.Title>
            </Modal.Header>

            <Modal.Body>
              <div className="form-group">
                <label htmlFor="public-permalink">Public link</label>
                <p className="help-block">
                  Anyone with this link will be able to view the query and its
                  results.
                </p>
                <div className="input-group">
                  <input
                    id="public-permalink"
                    className="form-control"
                    readOnly
                    value={this.publicPermalink()}
                    onFocus={this.selectAll}
                  />
                  <Button
                    variant="outline-secondary"
                    size="sm"
                    onClick={() => copyToClipboard("public-permalink")}
                  >
                    Copy link
                  </Button>
                </div>
              </div>

              <div className="form-group">
                <label htmlFor="private-permalink">Private link</label>
                <p className="help-block">
                  This link requires logging in with an Insights Air account
                  that is allowed to access this data source.
                </p>
                <div className="input-group">
                  <input
                    id="private-permalink"
                    className="form-control"
                    readOnly
                    value={this.privatePermalink()}
                    onFocus={this.selectAll}
                  />
                  <Button
                    variant="outline-secondary"
                    size="sm"
                    onClick={() => copyToClipboard("private-permalink")}
                  >
                    Copy link
                  </Button>
                </div>
              </div>
            </Modal.Body>
          </Modal>
        </>
      );
    } else {
      return null;
    }
  }
}
