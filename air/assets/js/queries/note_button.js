// @flow

import type { Node } from "React";
import React, { useEffect, useRef, useState } from "react";
import Modal from "react-bootstrap/Modal";
import Button from "react-bootstrap/Button";

type Props = {
  initialValue: string | null,
  onChange: (newValue: string) => void,
};

const selectAll = (event: any) => {
  event.target.select();
};

export default ({ initialValue, onChange }: Props): Node => {
  const inputRef = useRef();
  const [currentValue, setCurrentValue] = useState(initialValue || "");
  const [isOpen, setIsOpen] = useState(false);

  useEffect(() => {
    setCurrentValue(initialValue || "");
  }, [initialValue, isOpen]);

  useEffect(() => {
    if (isOpen && inputRef.current) {
      inputRef.current.focus();
    }
  }, [isOpen]);

  const handleClose = () => setIsOpen(false);
  const handleShow = () => setIsOpen(true);

  return (
    <>
      <button
        type="button"
        className="btn btn-sm float-right"
        onClick={handleShow}
      >
        <i className="far fa-comment-alt" aria-label="Set note"></i>
      </button>

      <Modal show={isOpen} onHide={handleClose} centered>
        <form
          onSubmit={(e) => {
            e.preventDefault();
            setIsOpen(false);
            onChange(currentValue);
          }}
        >
          <Modal.Header closeButton>
            <Modal.Title>Query note</Modal.Title>
          </Modal.Header>

          <Modal.Body>
            <div className="form-group">
              <div className="input-group">
                <input
                  ref={inputRef}
                  className="form-control"
                  value={currentValue}
                  onChange={(e) => setCurrentValue(e.target.value)}
                  onFocus={selectAll}
                />
              </div>
            </div>
          </Modal.Body>

          <Modal.Footer>
            <Button variant="secondary" onClick={handleClose}>
              Cancel
            </Button>
            <Button variant="primary" type="submit">
              Save
            </Button>
          </Modal.Footer>
        </form>
      </Modal>
    </>
  );
};
