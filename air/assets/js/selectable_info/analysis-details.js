import React, { useEffect } from "react";
import { formatNumber } from "../number_format";
import moment from "moment-timezone";
import { VegaLite } from "react-vega";
import { Handler } from "vega-tooltip";

const OTHER_ITEM = "--OTHER--";

const range = (numberFormat, type) => {
  const formatter = (value) =>
    type === "date" || type === "datetime"
      ? moment(value).format("LLL")
      : formatNumber(value, numberFormat);
  return (min, max) => (
    <div className="list-group list-group-horizontal mb-3">
      {niceBox("Minimum", formatter)(min)}
      {niceBox("Maximum", formatter)(max)}
    </div>
  );
};

const plot = (spec) => (
  <VegaLite
    width={Math.min(330, document.body.clientWidth - 150)}
    padding={0}
    actions={false}
    tooltip={new Handler().call}
    spec={{
      ...spec,
      autosize: {
        type: "fit-x",
        contains: "padding",
      },
    }}
  />
);

const filterOtherValue = (numberFormat, data) => {
  let totalCount = 0;
  let topCount = 0;
  const filteredItems = data.filter((item) => {
    totalCount += item.count;
    if (item.value !== OTHER_ITEM) {
      topCount += item.count;
      return true;
    }
    return false;
  });

  return {
    numItems: filteredItems.length,
    shownPercent: formatNumber((topCount / totalCount) * 100, numberFormat),
    data: filteredItems,
  };
};

const topValues = (numberFormat) => (data) => {
  let processedData = filterOtherValue(numberFormat, data);

  return (
    <div>
      <h4 className="text-muted font-weight-bold text-uppercase small">
        Top Values
      </h4>
      {plot({
        mark: {
          type: "bar",
        },
        data: {
          values: processedData.data,
        },
        encoding: {
          y: {
            field: "value",
            type: "ordinal",
            axis: {
              title: `Top ${processedData.numItems} values`,
            },
            sort: "-x",
          },
          x: {
            field: "count",
            type: "quantitative",
          },
        },
      })}
      <p className="small ml-3">
        The top {processedData.numItems} values account for about{" "}
        {processedData.shownPercent}% of the total number of records.
      </p>
    </div>
  );
};

const exactValues = (numberFormat) => (data) => {
  let processedData = filterOtherValue(numberFormat, data);
  return (
    <div>
      <h4 className="text-muted font-weight-bold text-uppercase small">
        Value Distribution
      </h4>
      {plot({
        mark: {
          type: "bar",
        },
        data: {
          values: processedData.data,
        },
        encoding: {
          y: {
            field: "value",
            type: "ordinal",
            axis: {
              title: false,
            },
          },
          x: {
            field: "count",
            type: "quantitative",
          },
        },
      })}
      <p className="small ml-3">
        The {processedData.numItems} values shown above account for about{" "}
        {processedData.shownPercent}% of the total number of records.
      </p>
    </div>
  );
};

const bucketed = (data) => (
  <div className="mb-3">
    <h4 className="text-muted font-weight-bold text-uppercase small">
      Histogram
    </h4>
    {plot({
      mark: {
        type: "bar",
      },
      data: {
        values: data,
      },
      transform: [
        { calculate: "datum.lowerBound + datum.bucketSize", as: "upperBound" },
      ],
      encoding: {
        x: {
          field: "lowerBound",
          bin: "binned",
          type: "quantitative",
          axis: { title: "value (binned)" },
        },
        x2: {
          field: "upperBound",

          type: "quantitative",
        },
        y: {
          field: "count",
          type: "quantitative",
        },
      },
    })}
  </div>
);

const boxplot = ([q1, q2, q3], min, max) => (
  <div className="mb-3">
    <h4 className="text-muted font-weight-bold text-uppercase small">
      Quartiles
    </h4>
    {plot({
      data: {
        values: [
          {
            min,
            q1,
            q2,
            q3,
            max,
          },
        ],
      },
      layer: [
        {
          mark: {
            type: "rule",
            invalid: null,
            style: "boxplot-rule",
            aria: false,
          },
          encoding: {
            x: {
              field: "min",
              type: "quantitative",
              axis: { title: false },
              scale: { zero: false },
            },
            x2: { field: "q1" },
          },
        },
        {
          mark: {
            type: "rule",
            invalid: null,
            style: "boxplot-rule",
            aria: false,
          },
          encoding: {
            x: {
              field: "q3",
              type: "quantitative",
            },
            x2: { field: "max" },
          },
        },
        {
          mark: {
            type: "bar",
            size: 14,
            orient: "horizontal",
            invalid: null,
            style: "boxplot-box",
            aria: false,
          },
          encoding: {
            x: {
              field: "q1",
              type: "quantitative",
            },
            x2: { field: "q3" },
          },
        },
        {
          mark: {
            color: "white",
            type: "tick",
            invalid: null,
            size: 14,
            orient: "vertical",
            ariaRoleDescription: "box",
            style: "boxplot-median",
          },
          encoding: {
            x: {
              field: "q2",
              type: "quantitative",
            },
          },
        },
      ],
    })}
  </div>
);

const simpleField = (title, formatter = (a) => a) => (value) => (
  <p className="d-flex justify-content-between align-items-baseline">
    <b className="text-muted font-weight-bold text-uppercase small">{title}</b>
    <span className="font-weight-bold">{formatter(value)}</span>
  </p>
);

const niceBox = (title, formatter = (a) => a) => (value) => (
  <div className="list-group-item d-flex flex-column flex-grow-1 align-items-center flex-basis-1 mt-2">
    <b className="text-muted font-weight-bold text-uppercase small">{title}</b>
    <span className="font-weight-bold">{formatter(value)}</span>
  </div>
);

const AnalysisDetails = ({ numberFormat, analysis, type, popper }) => {
  const renderIfPrereqs = (cb, ...keys) => {
    const match = analysis.filter((an) => keys.includes(an.name));
    if (match.length === keys.length) {
      return cb(...keys.map((key) => match.find((m) => m.name === key).value));
    }
    return null;
  };

  const formatNum = (num) => formatNumber(num, numberFormat);

  useEffect(() => {
    popper.scheduleUpdate();
  }, [popper]);

  return (
    <div>
      {renderIfPrereqs(
        range(numberFormat, type),
        "refined_min",
        "refined_max"
      ) || renderIfPrereqs(range(numberFormat, type), "naive_min", "naive_max")}
      {renderIfPrereqs(simpleField("Average", formatNum), "avg_estimate")}
      {renderIfPrereqs(
        boxplot,
        "quartile_estimates",
        "refined_min",
        "refined_max"
      )}
      {renderIfPrereqs(topValues(numberFormat), "distinct.top_values") ||
        renderIfPrereqs(exactValues(numberFormat), "distinct.values")}
      {renderIfPrereqs(bucketed, "histogram.buckets")}
      <div className="list-group list-group-horizontal mb-3">
        {renderIfPrereqs(
          niceBox("Total records", formatNum),
          "distinct.total_count"
        )}
        {renderIfPrereqs(
          niceBox("Suppressed", formatNum),
          "distinct.suppressed_count"
        )}
        {renderIfPrereqs(niceBox("Null", formatNum), "distinct.null_count")}
      </div>
      {renderIfPrereqs(
        simpleField("Last analyzed", (date) => moment.utc(date).fromNow()),
        "updated_at"
      )}
    </div>
  );
};

export default AnalysisDetails;
