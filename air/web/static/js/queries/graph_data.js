import _ from "lodash";

export class GraphData {
  constructor(rows, columns, valueFormatter = this.defaultValueFormatter) {
    this.rows = rows;
    this.columns = columns;
    this.formatValue = valueFormatter;

    this.traces = this.traces.bind(this);
    this.charteable = this.charteable.bind(this);
  }


  // ----------------------------------------------------------------
  // API
  // ----------------------------------------------------------------

  charteable() {
    return this.columns.length >= 2 &&
      this.rows.length > 1 &&
      this.rows.length <= 1000 &&
      this.hasNumericalColumn();
  }

  traces(mode) {
    const xAxisValues = this.xAxisValues();
    return _.flatMap(this.yColumns(), (value, _index, collection) =>
      this.produceTrace(value, collection, xAxisValues, mode));
  }


  // ----------------------------------------------------------------
  // Internal functions
  // ----------------------------------------------------------------

  hasNumericalColumn() {
    return _.some(this.rows[0].row, value => this.isNumeric(value));
  }

  isNumeric(n) {
    return typeof(n) === "number" && isFinite(n);
  }

  // Dummy implementation, replaced by called
  defaultValueFormatter(value) {
    return value;
  }


  produceTrace(value, collection, xAxisValues, mode) {
    const columnIndex = value.index;
    const columnName = value.name;
    const renderableValues = this.rows.map((accumulateRow) => accumulateRow.row[columnIndex]);
    const trace = {
      type: mode,
      name: columnName,
      y: renderableValues,
      x: xAxisValues,
      line: {color: value.colour.primary},
      marker: {color: value.colour.primary},
    };
    if (mode === "bar" && value.noise) {
      const yErrorData = this.rows.map((accumulateRow) => accumulateRow.row[value.noise.index]);
      trace.error_y = {
        type: "data",
        array: yErrorData,
        visible: true,
      };
      return [trace];
    } else if (mode === "line" && value.noise) {
      return [
        this.errorTraceWithSD(value, renderableValues, xAxisValues, 3, value.colour.error3, false),
        this.errorTraceWithSD(value, renderableValues, xAxisValues, 2, value.colour.error2, false),
        this.errorTraceWithSD(value, renderableValues, xAxisValues, 1, value.colour.error1, true),
        trace,
      ];
    } else {
      return [trace];
    }
  }

  errorTraceWithSD(value, yValues, xAxisValues, n, colour, showByDefault) {
    const yErrorData = this.rows.map((accumulateRow) => accumulateRow.row[value.noise.index]);
    const combinedData = _.zip(yValues, yErrorData);
    const forwardYValues = _.map(combinedData, ([a, b]) => a + n * b);
    const backwardYValues = _.map(_.reverse(combinedData), ([a, b]) => a - n * b);
    const errorYValues = _.concat(forwardYValues, backwardYValues);
    const errorXValues = _.concat(xAxisValues, _.reverse(_.clone(xAxisValues)));
    let trace = {
      x: errorXValues,
      y: errorYValues,
      fill: "tozerox",
      fillcolor: colour,
      line: {color: "transparent"},
      name: `${value.name} noise (${n} SDs)`,
      showlegend: true,
      type: "scatter",
    };
    if (! showByDefault) {
      trace.visible = "legendonly";
    }
    return trace;
  }

  nextColour(colourIndex) {
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
  }

  yColumns() {
    let noiseColumnsConsumed = 0;
    let colourIndex = 0;

    const usableColumns = _.chain(this.columns)
      .map((column, i) => {
        if (this.isNumeric(this.rows[0].row[i])) {
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

    var [noiseColumns, valueColumns] = _.partition(usableColumns, (column) => 
      _.endsWith(column.name, "_noise"));

    const valueColumnsWithNoise = _.map(valueColumns, column => {
      const noiseColumn = _.find(noiseColumns, {'name': `${column.name}_noise`});
      if (noiseColumn) {
        noiseColumns = _.without(noiseColumns, noiseColumn);
        column.noise = noiseColumn;
        noiseColumnsConsumed = noiseColumnsConsumed + 1;
      }
      return column
    });

    let renderableColumns = _.chain(valueColumnsWithNoise)
      // The noise columns that haven't already been consumed
      .concat(noiseColumns)
      .orderBy(column => column.index)
      .map(column => {
        column.colour = this.nextColour(colourIndex);
        colourIndex = colourIndex + 1;
        return column;
      })
      .value();

    // If all columns are eligible for being a y-column trace, then we'll make the first one the x-column.
    if (renderableColumns.length + noiseColumnsConsumed === this.columns.length) {
      return _.drop(renderableColumns, 1);
    } else {
      return renderableColumns;
    }
  }

  xAxisValues() {
    const yValueIndices = _.flatMap(this.yColumns(), v => {
      if (v.noise) {
        return [v.index, v.noise.index];
      } else {
        return [v.index];
      }
    });
    const xAxisValues = this.rows.map(accumulateRow => {
      let index = 0;
      const nonNumericalValues = _.reduce(accumulateRow.row, (acc, value) => {
        if (! _.includes(yValueIndices, index)) {
          acc.push(this.formatValue(value));
        }
        index = index + 1;
        return acc;
      }, []);
      return _.join(nonNumericalValues, ", ");
    });
    return xAxisValues;
  }
}


