import React from "react";
import { Popover, OverlayTrigger } from "react-bootstrap";
import { selectableType } from "./selectable-type";

const NUM_SAMPLES = 10;

const isAnalystCreatedSelectable = (selectable) => {
  return selectable.kind === "view" || selectable.kind === "analyst_table";
};

const editLinkUrl = (selectable, selectablesEditUrl) =>
  `${selectablesEditUrl}?kind=${selectable.kind}&id=${selectable.internal_id}`;

const hasSampleData = (selectable) => {
  return "sample_data" in selectable && selectable.sample_data.length > 0;
};

const analyzedColumns = (selectable) => {
  return selectable.columns.filter((column) => "analysis" in column);
};

const SelectableDetails = React.forwardRef(
  ({ selectable, triggerDelete, selectablesEditUrl, ...props }, ref) => {
    return (
      <Popover
        {...props}
        ref={ref}
        style={{
          ...props.style,
          minWidth: "250px",
          maxWidth: "calc(100vw - 100px)",
        }}
      >
        <Popover.Title>
          <div className="d-flex justify-content-between align-items-baseline">
            <h4 className="h6 m-0" style={{ wordBreak: "break-all" }}>
              <i className="fas fa-table" aria-label="Table"></i>{" "}
              {selectable.id}
            </h4>
            <button
              className="btn btn-link py-0 pr-0"
              disabled={!window.insertWordInEditor}
              onClick={(event) => {
                event.preventDefault();
                window.insertWordInEditor(`"${selectable.id}"`);
              }}
            >
              <i className="fas fa-reply" title="Insert into editor"></i>
            </button>
          </div>

          <small className="text-muted">{selectableType(selectable)}</small>
        </Popover.Title>
        {(selectable.comment ||
          isAnalystCreatedSelectable(selectable) ||
          hasSampleData(selectable)) && (
          <Popover.Content>
            <div style={{ maxHeight: "80vh", overflowY: "auto" }}>
              {selectable.comment && (
                <div className="border rounded px-2 py-1">
                  <strong className="float-right text-muted ml-3 mt-1 font-weight-bold text-uppercase small">
                    Comment
                  </strong>
                  <div style={{ whiteSpace: "pre-line" }}>
                    {selectable.comment}
                  </div>
                </div>
              )}

              {isAnalystCreatedSelectable(selectable) && (
                <div className="pt-3 d-flex justify-content-around">
                  <a
                    className="btn btn-sm btn-secondary"
                    style={{ flexBasis: "35%" }}
                    href={editLinkUrl(selectable, selectablesEditUrl)}
                  >
                    Edit
                  </a>
                  <button
                    type="button"
                    style={{ flexBasis: "35%" }}
                    className="btn btn-sm btn-danger"
                    onClick={triggerDelete}
                  >
                    Delete
                  </button>
                </div>
              )}

              {hasSampleData(selectable) && (
                <div className="pt-3">
                  <h4 className="text-muted font-weight-bold text-uppercase small">
                    Sample values
                  </h4>
                  <table className="table">
                    <caption className="small">
                      The sample values were synthesized from the{" "}
                      <strong>{selectable.id}</strong> table.
                    </caption>
                    <thead>
                      <tr>
                        {analyzedColumns(selectable).map((column) => {
                          return <th key={column.name}>{column.name}</th>;
                        })}
                      </tr>
                    </thead>
                    <tbody>
                      {selectable.sample_data
                        .slice(0, NUM_SAMPLES)
                        .map((row, i) => {
                          return (
                            <tr key={`sample-row-${i}`}>
                              {row.map((column, j) => {
                                return (
                                  <td key={`sample-row-${i}-column-${j}`}>
                                    {column === null ? <em>null</em> : column}
                                  </td>
                                );
                              })}
                            </tr>
                          );
                        })}
                    </tbody>
                  </table>
                </div>
              )}
            </div>
          </Popover.Content>
        )}
      </Popover>
    );
  }
);

const SelectableInfo = (props) => {
  return (
    <OverlayTrigger
      trigger="click"
      placement="left"
      rootClose={true}
      overlay={<SelectableDetails {...props} />}
    >
      <button className="btn p-1 text-muted float-right">
        <i className="fas fa-info-circle" aria-label="Details"></i>
      </button>
    </OverlayTrigger>
  );
};

export default SelectableInfo;
