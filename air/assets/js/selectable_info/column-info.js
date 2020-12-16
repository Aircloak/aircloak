import React from "react";
import { Popover, OverlayTrigger } from "react-bootstrap";
import loader from "../../static/images/loader.gif";
import ColumnIcon from "./column-icon";

const AnalysisDetailsContent = React.lazy(() => import("./analysis-details"));

const isolationLabel = (isolated) => {
  switch (isolated) {
    case true:
    case null:
      return "Yes";
    case false:
      return "No";
    case "pending":
      return "Pending";
    case "failed":
      return "Failed";
    default:
      return "Yes";
  }
};

const AnalysisDetails = React.forwardRef(
  ({ item, numberFormat, ...props }, ref) => {
    return (
      <Popover
        {...props}
        ref={ref}
        style={{
          ...props.style,
          minWidth: "200px",
          maxWidth: "min(380px, calc(100vw - 100px))",
        }}
      >
        <Popover.Title className="d-flex justify-content-between align-items-baseline">
          <h4 className="h6 m-0" style={{ wordBreak: "break-all" }}>
            <ColumnIcon column={item} /> {item.name}
          </h4>
          <button
            className="btn btn-link"
            disabled={!window.insertWordInEditor}
            onClick={(event) => {
              event.preventDefault();
              window.insertWordInEditor(`"${item.name}"`);
            }}
          >
            <i className="fas fa-reply" title="Insert into editor"></i>
          </button>
        </Popover.Title>
        <div
          className="bg-light rounded-bottom"
          style={{ maxHeight: "50vh", overflowY: "auto" }}
        >
          {item.comment && (
            <Popover.Content className="bg-white mb-3">
              <div className="border rounded px-2 py-1 m-0 ">
                <strong className="float-right text-muted ml-3 mt-1 font-weight-bold text-uppercase small">
                  Comment
                </strong>
                <div style={{ whiteSpace: "pre-line" }}>{item.comment}</div>
              </div>
            </Popover.Content>
          )}
          <Popover.Content className="bg-white px-2">
            <div className="list-group list-group-horizontal">
              <div className="list-group-item d-flex flex-column flex-grow-1 align-items-center flex-basis-1">
                <b className="text-muted font-weight-bold text-uppercase small">
                  Type
                </b>
                <span className="font-weight-bold">{item.type}</span>
              </div>
              {item.access !== undefined && (
                <div className="list-group-item d-flex flex-column flex-grow-1 align-items-center flex-basis-1">
                  <b className="text-muted font-weight-bold text-uppercase small">
                    Selectable
                  </b>
                  <span className="font-weight-bold">
                    {item.access === "unselectable" ? "No" : "Yes"}
                  </span>
                </div>
              )}
              {item.isolated !== undefined && (
                <div className="list-group-item d-flex flex-column flex-grow-1 align-items-center flex-basis-1">
                  <b className="text-muted font-weight-bold text-uppercase small">
                    Isolates
                  </b>
                  <span className="font-weight-bold">
                    {isolationLabel(item.isolated)}
                  </span>
                </div>
              )}
            </div>
          </Popover.Content>

          {item.analysis && (
            <Popover.Content className="bg-white mt-3 px-2">
              <React.Suspense
                fallback={
                  <img
                    alt="Icon indicating loading of analysis details"
                    src={loader}
                  />
                }
              >
                <AnalysisDetailsContent
                  name={item.name}
                  analysis={item.analysis}
                  numberFormat={numberFormat}
                  type={item.type}
                  popper={props.popper}
                />
              </React.Suspense>
            </Popover.Content>
          )}
        </div>
      </Popover>
    );
  }
);

const ColumnInfo = (props) => {
  return (
    <OverlayTrigger
      trigger="click"
      placement="left"
      rootClose={true}
      overlay={<AnalysisDetails {...props} />}
    >
      <button className="btn p-1 text-muted">
        <i className="fas fa-info-circle" aria-label="Details"></i>
      </button>
    </OverlayTrigger>
  );
};

export default ColumnInfo;
