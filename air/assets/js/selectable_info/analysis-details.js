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
      {niceBox("Minimum", formatter(min))}
      {niceBox("Maximum", formatter(max))}
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

const exactValues = (formatNum) => (
  data,
  totalCount,
  nullCount,
  suppressedCount
) => {
  const values = data.filter((item) => item.value !== OTHER_ITEM);
  const shownCount = data.reduce((sum, item) => sum + item.count, 0);
  const percentage = (shownCount / totalCount) * 100;
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
          values,
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
          tooltip: {
            field: "count",
            type: "quantitative",
          },
        },
      })}
      <p className="small ml-3">
        The {values.length} values shown above account for about{" "}
        {formatNum(percentage)}% of the total number of records.{" "}
        {nullCount > 0 &&
          `There were about ${formatNum(nullCount)} null records.`}
        {suppressedCount > 0 &&
          `About ${suppressedCount} records were suppressed when generating this analysis.`}
      </p>
    </div>
  );
};

const bucketed = (formatNum) => (
  data,
  { nullCount, suppressedCount, suppressedCountRatio }
) => (
  <div className="mb-3">
    <h4 className="text-muted font-weight-bold text-uppercase small">
      Histogram
    </h4>
    {plot({
      mark: {
        type: "bar",
        tooltip: true,
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
    <p className="small ml-3">
      {nullCount > 0 &&
        `There were about ${formatNum(
          nullCount
        )} null records not shown above. `}
      {suppressedCount > 0 &&
        `About ${formatNum(
          suppressedCountRatio * 100
        )}% of records were suppressed and are not shown above.`}
    </p>
  </div>
);

const textLenghts = (formatNum) => (
  data,
  { nullCount, suppressedCount, suppressedCountRatio }
) => (
  <div>
    <h4 className="text-muted font-weight-bold text-uppercase small">
      String Length
    </h4>
    {plot({
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
        tooltip: {
          field: "count",
          type: "quantitative",
        },
      },
    })}
    <p className="small ml-3">
      {nullCount > 0 &&
        `There were about ${formatNum(
          nullCount
        )} null records not shown above. `}
      {suppressedCount > 0 &&
        `About ${formatNum(
          suppressedCountRatio * 100
        )}% of records were suppressed and are not shown above.`}
    </p>
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

const niceBox = (title, value) => (
  <div className="list-group-item d-flex flex-column flex-grow-1 align-items-center flex-basis-1">
    <b className="text-muted font-weight-bold text-uppercase small">{title}</b>
    <span className="font-weight-bold">{value}</span>
  </div>
);

const descriptiveStats = (formatNum) => (
  { entropy, mean, mode, quartiles, standardDeviation, variance },
  min,
  max
) => (
  <div className="stats-group mb-3">
    <h4 className="text-muted font-weight-bold text-uppercase small">
      Descriptive Stats
    </h4>
    <div className="list-group list-group-horizontal">
      {niceBox("Minimum", formatNum(min))}
      {niceBox("Median", formatNum(quartiles[1]))}
      {niceBox("Maximum", formatNum(max))}
    </div>
    <div className="list-group list-group-horizontal">
      {niceBox("Mean", formatNum(mean))}
      {niceBox("Mode", formatNum(mode))}
    </div>
    <div className="list-group list-group-horizontal">
      {niceBox("Entropy", formatNum(entropy))}
      {niceBox("StdDev", formatNum(standardDeviation))}
      {niceBox("Variance", formatNum(variance))}
    </div>
  </div>
);

const AnalysisDetails = ({ numberFormat, analysis, type, popper }) => {
  /**
   * This function will call the rendering function passed as the first argument,
   * with values fetched from the analysis in their respective positions. However,
   * if the analysis doesn't contain those keys, it will return null.
   *
   * Hence this allows us to conditionally render appropriate visualizations if the
   * analysis containes the necessary data to support that visualization.
   */
  const renderIfPrereqs = (cb, ...keys) => {
    const match = analysis.filter((an) => keys.includes(an.name));
    if (match.length === keys.length) {
      return cb(...keys.map((key) => match.find((m) => m.name === key).value));
    }
    return null;
  };

  const formatNum = (num) => formatNumber(num, numberFormat);

  useEffect(() => {
    // this will resize and reposition the popup.
    // necessary due to the lazy loading of this code.
    popper.scheduleUpdate();
  }, [popper]);

  return (
    <div>
      {renderIfPrereqs(
        descriptiveStats(formatNum),
        "descriptive_stats",
        "refined_min",
        "refined_max"
      ) ||
        renderIfPrereqs(
          range(numberFormat, type),
          "refined_min",
          "refined_max"
        )}
      {renderIfPrereqs(
        boxplot,
        "quartile_estimates",
        "refined_min",
        "refined_max"
      )}
      {renderIfPrereqs(
        exactValues(formatNum),
        "distinct.values",
        "distinct.value_count",
        "distinct.null_count",
        "distinct.suppressed_count"
      )}
      {renderIfPrereqs(
        bucketed(formatNum),
        "histogram.buckets",
        "histogram.value_counts"
      )}
      {renderIfPrereqs(
        textLenghts(formatNum),
        "text.length.values",
        "text.length.counts"
      )}

      {renderIfPrereqs(
        simpleField("Last analyzed", (date) => moment.utc(date).fromNow()),
        "updated_at"
      )}
    </div>
  );
};

export default AnalysisDetails;
