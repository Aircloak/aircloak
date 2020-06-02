import React from "react";
import { formatNumber } from "../number_format";
import moment from "moment-timezone";
import { VegaLite } from "react-vega";

const range = (numberFormat, type) => (min, max) => (
  <p>
    <b>Extent:</b>{" "}
    {type === "date" || type === "datetime"
      ? `${moment(min).format("LLL")} - ${moment(max).format("LLL")} `
      : `${formatNumber(min, numberFormat)} - ${formatNumber(
          max,
          numberFormat
        )}`}
  </p>
);

const exactValues = (data) => (
  <VegaLite
    width={180}
    padding={10}
    actions={false}
    spec={{
      mark: {
        type: "bar",
      },
      data: {
        values: data,
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
    }}
  />
);

const bucketed = (data) => (
  <VegaLite
    width={180}
    padding={10}
    actions={false}
    spec={{
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
    }}
  />
);

const boxplot = ([q1, q2, q3], min, max) => (
  <VegaLite
    width={230}
    padding={10}
    actions={false}
    spec={{
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
              axis: { title: "quartiles" },
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
    }}
  />
);

const simpleField = (title, formatter = (a) => a) => (value) => (
  <p>
    <b>{title}:</b> {formatter(value)}
  </p>
);

const AnalysisDetails = ({ numberFormat, analysis, type }) => {
  const renderIfPrereqs = (cb, ...keys) => {
    const match = analysis.filter((an) => keys.includes(an.name));
    if (match.length === keys.length) {
      return cb(...keys.map((key) => match.find((m) => m.name === key).value));
    }
    return null;
  };

  const formatNum = (num) => formatNumber(num, numberFormat);

  return (
    <div>
      <p>
        <b>Type:</b> {type}
      </p>
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
      {renderIfPrereqs(exactValues, "distinct.top_values") ||
        renderIfPrereqs(exactValues, "distinct.values")}
      {renderIfPrereqs(bucketed, "histogram.buckets")}
      {renderIfPrereqs(
        simpleField("Total records", formatNum),
        "distinct.total_count"
      )}
      {renderIfPrereqs(
        simpleField("Suppressed", formatNum),
        "distinct.suppressed_count"
      )}
      {renderIfPrereqs(simpleField("Null", formatNum), "distinct.null_count")}
      {renderIfPrereqs(
        simpleField("Last updated", (date) => moment(date).fromNow()),
        "updated_at"
      )}
    </div>
  );
};

export default AnalysisDetails;
