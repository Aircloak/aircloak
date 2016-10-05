// @flow

import _ from "lodash";
import type {Row, Column} from "./result";

export type GraphDataT = {
  charteable: () => boolean,
  traces: (mode: string) => any[],
  xAxisLabel: () => string,
};

type ValueFormatter = (value: any) => any;

export const GraphData = (rows: Row[], columns: Column[], valueFormatter: ValueFormatter) => {
  // We use our own default value formatter if none is provided
  const formatValue = valueFormatter || ((v) => v);


  // ----------------------------------------------------------------
  // Internal functions
  // ----------------------------------------------------------------

  const isNumeric = (n) => typeof(n) === "number" && isFinite(n);

  const hasNumericalColumn = () => _.some(rows[0].row, value => isNumeric(value));

  const errorTraceWithSD = (value, yValues, xAxisValues, n, colour, showByDefault) => {
    const yErrorData = rows.map((accumulateRow) => accumulateRow.row[value.noise.index]);
    const combinedData = _.zip(yValues, yErrorData);
    const forwardYValues = _.map(combinedData, ([a, b]) => a + n * b);
    const backwardYValues = _.map(_.reverse(combinedData), ([a, b]) => a - n * b);
    const errorYValues = _.concat(forwardYValues, backwardYValues);
    const errorXValues = _.concat(xAxisValues, _.reverse(_.clone(xAxisValues)));
    const trace = {
      x: errorXValues,
      y: errorYValues,
      fill: "tozerox",
      fillcolor: colour,
      line: {color: "transparent"},
      name: `${value.name} noise (${n} SDs)`,
      showlegend: true,
      visible: "true",
      type: "scatter",
    };
    if (! showByDefault) {
      trace.visible = "legendonly";
    }
    return trace;
  };

  const produceTrace = (value, xAxisValues, mode) => {
    const columnIndex = value.index;
    const columnName = value.name;
    const renderableValues = rows.map((accumulateRow) => accumulateRow.row[columnIndex]);
    const trace = {
      type: mode,
      name: columnName,
      y: renderableValues,
      x: xAxisValues,
      line: {color: value.colour.primary},
      marker: {color: value.colour.primary},
      error_y: {},
    };
    if (mode === "bar" && value.noise) {
      const yErrorData = rows.map((accumulateRow) => accumulateRow.row[value.noise.index]);
      trace.error_y = {
        type: "data",
        array: yErrorData,
        visible: true,
      };
      return [trace];
    } else if (mode === "line" && value.noise) {
      return [
        errorTraceWithSD(value, renderableValues, xAxisValues, 3, value.colour.error3, false),
        errorTraceWithSD(value, renderableValues, xAxisValues, 2, value.colour.error2, false),
        errorTraceWithSD(value, renderableValues, xAxisValues, 1, value.colour.error1, true),
        trace,
      ];
    } else {
      return [trace];
    }
  };

  const nextColour = (colourIndex) => {
    const colours = [
      {
        primary: "rgb(110,110,110)",
        error1: "rgba(147,147,147,0.3)",
        error2: "rgba(183,183,183,0.3)",
        error3: "rgba(219,219,219,0.3)",
      },
      {
        primary: "rgb(33,139,150)",
        error1: "rgba(89,168,176,0.3)",
        error2: "rgba(194,197,202,0.3)",
        error3: "rgba(200,226,229,0.3)",
      },
      {
        primary: "rgb(0,170,150)",
        error1: "rgba(64,192,176,0.3)",
        error2: "rgba(128,213,202,0.3)",
        error3: "rgba(191,234,229,0.3)",
      },
      {
        primary: "rgb(148,193,26)",
        error1: "rgba(179,207,94,0.3)",
        error2: "rgba(201,224,140,0.3)",
        error3: "rgba(228,239,198,0.3)",
      },
      {
        primary: "rgb(30,185,214)",
        error1: "rgba(124,203,225,0.3)",
        error2: "rgba(142,220,234,0.3)",
        error3: "rgba(199,237,245,0.3)",
      },
    ];
    const usableColourIndex = colourIndex % colours.length;
    return colours[usableColourIndex];
  };

  const yColumns = () => {
    let noiseColumnsConsumed = 0;
    let colourIndex = 0;

    const usableColumns = _.chain(columns)
      .map((column, i) => {
        if (isNumeric(rows[0].row[i])) {
          return {
            index: i,
            name: column,
          };
        } else {
          return null;
        }
      })
      .filter((e) => e != null)
      .value();

    const partitionedValues = _.partition(usableColumns, (column) =>
      _.endsWith(column.name, "_noise"));
    let noiseColumns = partitionedValues[0];
    const valueColumns = partitionedValues[1];

    const valueColumnsWithNoise = _.map(valueColumns, column => {
      const noiseColumn = _.find(noiseColumns, {name: `${column.name}_noise`});
      if (noiseColumn) {
        noiseColumns = _.without(noiseColumns, noiseColumn);
        noiseColumnsConsumed = noiseColumnsConsumed + 1;
        const clonedColumn = _.clone(column);
        clonedColumn.noise = noiseColumn;
        return clonedColumn;
      } else {
        return column;
      }
    });

    const renderableColumns = _.chain(valueColumnsWithNoise)
      // The noise columns that haven't already been consumed
      .concat(noiseColumns)
      .orderBy(column => column.index)
      .map(column => {
        const clonedColumn = _.clone(column);
        clonedColumn.colour = nextColour(colourIndex);
        colourIndex = colourIndex + 1;
        return clonedColumn;
      })
      .value();

    // If all columns are eligible for being a y-column trace, then we'll make the first one the x-column.
    if (renderableColumns.length + noiseColumnsConsumed === columns.length) {
      return _.drop(renderableColumns, 1);
    } else {
      return renderableColumns;
    }
  };

  const yValueIndices = () =>
    _.flatMap(yColumns(), v => {
      if (v.noise) {
        return [v.index, v.noise.index];
      } else {
        return [v.index];
      }
    });

  const produceXAxisValues = () => {
    const yIndices = yValueIndices();
    const xAxisValues = rows.map(accumulateRow => {
      let index = 0;
      const nonNumericalValues = _.reduce(accumulateRow.row, (acc, value) => {
        if (! _.includes(yIndices, index)) {
          acc.push(formatValue(value));
        }
        index = index + 1;
        return acc;
      }, []);
      return _.join(nonNumericalValues, ", ");
    });
    return xAxisValues;
  };


  // ----------------------------------------------------------------
  // Exportable API functions
  // ----------------------------------------------------------------

  const charteable = (): boolean =>
    columns.length >= 2 &&
    rows.length > 1 &&
    rows.length <= 1000 &&
    hasNumericalColumn();

  const traces = (mode: string): any[] => {
    const xAxisValues = produceXAxisValues();
    return _.flatMap(yColumns(), value => produceTrace(value, xAxisValues, mode));
  };

  const xAxisLabel = (): string => {
    const yIndices = yValueIndices();
    return _.chain(columns)
      .reject((value, index) => _.includes(yIndices, index))
      .join(", ")
      .value();
  };

  return {charteable, traces, xAxisLabel};
};
