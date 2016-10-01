import React from "react";
import _ from "lodash";

import Plotly from "../plotly.js";
import {CodeViewer} from "../code_viewer";
import {Info} from "./info";

export class Result extends React.Component {
  constructor(props) {
    super(props);

    this.minRowsToShow = 10;

    this.state = {
      rowsToShowCount: this.minRowsToShow,
      showChart: false,
      mode: "bar",
    };

    this.handleClickMoreRows = this.handleClickMoreRows.bind(this);
    this.handleClickLessRows = this.handleClickLessRows.bind(this);

    this.renderRows = this.renderRows.bind(this);
    this.renderShowAll = this.renderShowAll.bind(this);
    this.renderOptionMenu = this.renderOptionMenu.bind(this);

    this.conditionallyRenderChart = this.conditionallyRenderChart.bind(this);
    this.setChartDataOnRef = this.setChartDataOnRef.bind(this);
    this.plotChart = this.plotChart.bind(this);
    this.changeGraphType = this.changeGraphType.bind(this);
    this.produceTrace = this.produceTrace.bind(this);

    this.showingAllOfFewRows = this.showingAllOfFewRows.bind(this);
    this.showingAllOfManyRows = this.showingAllOfManyRows.bind(this);
    this.showingMinimumNumberOfManyRows = this.showingMinimumNumberOfManyRows.bind(this);
  }

  componentDidUpdate() {
    this.plotChart();
  }

  setChartDataOnRef(ref) {
    this.chartRef = ref;
    this.plotChart();
  }

  plotChart() {
    if (! this.state.showChart || ! this.chartRef) {
      return;
    }
    const yValueIndices = _.flatMap(this.yColumns(), (v) => {
      if (v.noise) {
        return [v.index, v.noise.index];
      } else {
        return [v.index];
      }
    });
    const xAxisValues = this.props.rows.map((accumulateRow) => {
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
    const traces = _.flatMap(this.yColumns(), (value, _index, collection) =>
      this.produceTrace(value, collection, xAxisValues));
    const layout = {
      margin: {
        r: 0,
        b: 0,
        t: 0,
      },
      showlegend: true,
      legend: {
        orientation: "h",
      },
    };
    const displayOptions = {
      staticPlot: true,
    };
    Plotly.newPlot(this.chartRef, traces, layout, displayOptions);
  }

  produceTrace(value, collection, xAxisValues) {
    const columnIndex = value.index;
    const columnName = value.name;
    const renderableValues = this.props.rows.map((accumulateRow) => accumulateRow.row[columnIndex]);
    const trace = {
      type: this.state.mode,
      name: columnName,
      y: renderableValues,
      x: xAxisValues,
      line: {color: value.colour.primary},
      marker: {color: value.colour.primary},
    };
    if (this.state.mode === "bar" && value.noise) {
      const yErrorData = this.props.rows.map((accumulateRow) => accumulateRow.row[value.noise.index]);
      trace.error_y = {
        type: "data",
        array: yErrorData,
        visible: true,
      };
      return [trace];
    } else if (this.state.mode === "line" && value.noise) {
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
    const yErrorData = this.props.rows.map((accumulateRow) => accumulateRow.row[value.noise.index]);
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

  nextAvailableColour() {
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
    if (! this.colourIndex) {
      this.colourIndex = 0;
    }
    this.colourIndex = (this.colourIndex + 1) % colours.length;
    return colours[this.colourIndex];
  }

  handleClickMoreRows() {
    this.setState({rowsToShowCount: Math.min(this.state.rowsToShowCount * 2, this.props.row_count)});
  }

  handleClickLessRows() {
    const rowsToShowCount = Math.max(Math.round(this.state.rowsToShowCount / 2), this.minRowsToShow);
    this.setState({rowsToShowCount});
  }

  showingAllOfFewRows() {
    return this.props.row_count < this.minRowsToShow;
  }

  showingAllOfManyRows() {
    return this.props.row_count === this.state.rowsToShowCount;
  }

  showingMinimumNumberOfManyRows() {
    return this.state.rowsToShowCount === this.minRowsToShow && this.props.row_count > this.minRowsToShow;
  }

  formatValue(value) {
    if (value === null) {
      return "<null>";
    } else if (this.isNumeric(value)) {
      return Math.round(value * 1000) / 1000; // keep 3 decimals at most
    } else {
      return value.toString();
    }
  }

  isNumeric(n) {
    return typeof(n) === "number" && isFinite(n);
  }

  yColumns() {
    const columns = this.props.columns.map((column, i) => {
      if (this.isNumeric(this.props.rows[0].row[i])) {
        return {
          index: i,
          name: column,
        };
      } else {
        return null;
      }
    });
    let renderableColumnsUsed = 0;
    const preppedColumns = _.chain(columns)
      .filter((e) => e != null)
      .reduce((acc, column) => {
        renderableColumnsUsed = renderableColumnsUsed + 1;
        if (_.endsWith(column.name, "_noise")) {
          const namePart = column.name.substring(0, column.name.length - 6);
          const value = _.find(acc, col => col.name === namePart && col.noise === undefined);
          if (value) {
            value.noise = column;
          } else {
            acc.push(column);
          }
        } else {
          acc.push(column);
        }
        return acc;
      }, [])
      .map(column => {
        column.colour = this.nextAvailableColour();
        return column;
      })
      .value();
    // If all columns are eligible for being a y-column trace, then we'll make the first one the x-column.
    if (renderableColumnsUsed === this.props.columns.length) {
      return _.drop(preppedColumns, 1);
    } else {
      return preppedColumns;
    }
  }

  canShowChart() {
    return this.props.columns.length >= 2 &&
      this.props.rows.length > 1 &&
      this.props.rows.length < 500 &&
      this.yColumns().length > 0;
  }

  changeGraphType(e) {
    this.setState({mode: e.target.value});
  }

  conditionallyRenderChart() {
    if (this.state.showChart) {
      return (
        <div>
          Show as: <select onChange={this.changeGraphType}>
            <option value="bar">Bar chart</option>
            <option value="line">Line chart</option>
          </select>
          <div
            ref={this.setChartDataOnRef}
            className="plotlyGraph" style={{width: "100%", height: "500px"}}
          />
        </div>
      );
    } else {
      return null;
    }
  }

  renderRows() {
    let remainingRowsToProduce = this.state.rowsToShowCount;
    return _.flatMap(this.props.rows, (accumulateRow, i) => {
      const occurrencesForAccumulateRow = Math.min(remainingRowsToProduce, accumulateRow.occurrences);
      return _.range(occurrencesForAccumulateRow).map((occurrenceCount) => {
        remainingRowsToProduce -= 1;
        return (<tr key={`${i}-${occurrenceCount}`}>
          {accumulateRow.row.map((value, j) => <td key={j}>{this.formatValue(value)}</td>)}
        </tr>);
      });
    });
  }

  renderShowAll() {
    if (this.showingAllOfFewRows()) {
      return (
        <div className="row-count">
          {this.props.row_count} rows.
        </div>
      );
    } else if (this.showingAllOfManyRows()) {
      return (
        <div className="row-count">
          {this.props.row_count} rows.&nbsp;
          <a onClick={this.handleClickLessRows}>Show fewer rows</a>
        </div>
      );
    } else if (this.showingMinimumNumberOfManyRows()) {
      return (
        <div className="row-count">
          Showing {this.minRowsToShow} of {this.props.row_count} rows.&nbsp;
          <a onClick={this.handleClickMoreRows}>Show more rows</a>
        </div>
      );
    } else {
      const rowsShown = Math.min(this.state.rowsToShowCount, this.props.row_count);
      return (
        <div className="row-count">
          Showing {rowsShown} of {this.props.row_count} rows.&nbsp;
          Show&nbsp;
          <a onClick={this.handleClickLessRows}>fewer rows</a>,&nbsp;
          <a onClick={this.handleClickMoreRows}>more rows</a>
        </div>
      );
    }
  }

  renderChartButton() {
    if (this.canShowChart()) {
      const chartButtonText = this.state.showChart ? "Hide chart" : "Show chart";
      return (
        <button
          className="btn btn-default btn-xs"
          onClick={() => this.setState({showChart: ! this.state.showChart})}
        >
          {chartButtonText}
        </button>
      );
    } else {
      return null;
    }
  }

  renderOptionMenu() {
    return (
      <div className="options-menu">
        <a className="btn btn-default btn-xs" href={`/queries/${this.props.id}.csv`}>Download as CSV</a>
        &nbsp;
        {this.renderChartButton()}
      </div>
    );
  }

  render() {
    return (
      <div className="panel panel-success">
        <div className="panel-heading" />
        <div className="panel-body">
          <CodeViewer statement={this.props.statement} />
          <Info info={this.props.info} />
          <table className="table table-striped table-hover">
            <thead>
              <tr>
                {this.props.columns.map((column, i) =>
                  <th key={i}>{column}</th>
                )}
              </tr>
            </thead>

            <tbody>
              {this.renderRows()}
            </tbody>
          </table>
          {this.renderShowAll()}
          {this.renderOptionMenu()}
          {this.conditionallyRenderChart()}
        </div>
      </div>
    );
  }

}

Result.propTypes = {
  id: React.PropTypes.string,
  statement: React.PropTypes.string,
  columns: React.PropTypes.arrayOf(React.PropTypes.string).isRequired,
  rows: React.PropTypes.arrayOf(React.PropTypes.shape({
    occurrences: React.PropTypes.integer,
    row: React.PropTypes.array,
  })).isRequired,
  row_count: React.PropTypes.number,
  info: Info.propTypes.info,
};
