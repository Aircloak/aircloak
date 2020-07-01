import React from "react";
import { Popover, OverlayTrigger } from "react-bootstrap";
import loader from "../../static/images/loader.gif";

const AnalysisDetailsContent = React.lazy(() => import("./analysis-details"));

const AnalysisDetails = React.forwardRef(
  ({ name, analysis, type, numberFormat, ...props }, ref) => {
    return (
      <Popover
        {...props}
        ref={ref}
        style={{ ...props.style, maxWidth: "min(360px, calc(100vw - 100px))" }}
      >
        <Popover.Title>{name} - Diffix Explorer Analysis</Popover.Title>
        <Popover.Content>
          <div style={{ maxHeight: "50vh", overflowY: "auto" }}>
            <React.Suspense
              fallback={
                <img
                  alt="Icon indicating loading of analysis details"
                  src={loader}
                />
              }
            >
              <AnalysisDetailsContent
                name={name}
                analysis={analysis}
                numberFormat={numberFormat}
                type={type}
                popper={props.popper}
              />
            </React.Suspense>
          </div>
        </Popover.Content>
      </Popover>
    );
  }
);

const ExplorerResultButton = ({ numberFormat, item }) => {
  return (
    <OverlayTrigger
      trigger="click"
      placement="left"
      rootClose={true}
      overlay={
        <AnalysisDetails
          name={item.name}
          analysis={item.analysis}
          type={item.type}
          numberFormat={numberFormat}
        />
      }
    >
      <button className="btn p-1">
        <i
          className="fas fa-chart-bar"
          aria-label="Explorer Analysis Results"
        ></i>
      </button>
    </OverlayTrigger>
  );
};

export default ExplorerResultButton;
