import React from "react";
import { Popover, OverlayTrigger } from "react-bootstrap";
import loader from "../../static/images/loader.gif";

const AnalysisDetailsContent = React.lazy(() => import("./analysis-details"));

const AnalysisDetails = ({ name, analysis, type }, numberFormat, props) => {
  return (
    <Popover {...props}>
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
            />
          </React.Suspense>
        </div>
      </Popover.Content>
    </Popover>
  );
};

const ExplorerResultButton = ({ numberFormat, item }) => {
  return (
    <OverlayTrigger
      trigger="click"
      placement="left"
      rootClose={true}
      overlay={(props) => AnalysisDetails(item, numberFormat, props)}
    >
      <button className="btn btn-outline-secondary">
        <i
          className="fas fa-chart-bar"
          aria-label="Explorer Analysis Results"
        ></i>
      </button>
    </OverlayTrigger>
  );
};

export default ExplorerResultButton;
