// @flow

import React from "react";
import _ from "lodash";

import {AuthContext} from "../authentication_provider";
import {CodeViewer} from "../code_viewer";
import {Info} from "./info";
import {GraphData, GraphInfo, GraphConfig} from "./graph_data";
import {GraphConfigView} from "./graph_config_view";
import {GraphView} from "./graph_view";
import type {GraphDataT, GraphInfoT} from "./graph_data";
import {TableAligner} from "./table_aligner";
import type {TableAlignerT} from "./table_aligner";
import type {NumberFormat} from "../number_format";
import {formatNumber} from "../number_format";
import {loadBuckets} from "../request";
import {DebugExport} from "./debug_export";
import {ShareButton} from "./share_button";
import {activateTooltips} from "../tooltips";

export type Row = {
  occurrences: number,
  row: any[],
  unreliable: boolean,
};

export type Column = string;
export type Type = string;

export type Result = SuccessResult | PendingResult | CancelledResult | ErrorResult;

type CommonResultFeatures = {
  id: string,
  statement: string,
  data_source: {
    name: string,
  },
  private_permalink: ?string,
  public_permalink: ?string,
  session_id: ?string,
  inserted_at: ?string,
}

export type SuccessResult = CommonResultFeatures & {
  query_state: "completed",
  columns: Column[],
  types: Type[],
  rows: Row[],
  row_count: number,
  info: string[],
  user: {
    name: string,
  },
  inserted_at: string,
  buckets_link: string,
};

export type PendingResult = CommonResultFeatures & {
  query_state: "created",
}

export type CancelledResult = CommonResultFeatures & {
  query_state: "cancelled",
}

export type ErrorResult = CommonResultFeatures & {
  query_state: "error",
  error: string,
  info: string[]
};

type Props = {
  result: SuccessResult,
  numberFormat: NumberFormat,
  debugModeEnabled: boolean
};

type State = {
  rowsToShowCount: number,
  showChart: boolean,
  showChartConfig: boolean,
  graphConfig: GraphConfig,
  tableAligner: TableAlignerT,
  availableRows: Row[],
  availableChunks: number,
  loadingChunks: boolean,
  loadError: boolean,
};

const ZERO_WIDTH_SPACE = "\u200B";
const ALL_CHUNKS = -1;

export class ResultView extends React.Component<Props, State> {
  constructor(props: Props) {
    super(props);

    this.minRowsToShow = 10;

    this.state = {
      rowsToShowCount: this.minRowsToShow,
      showChart: false,
      showChartConfig: true,
      graphConfig: new GraphConfig(),
      tableAligner: new TableAligner(this.props.result.rows),
      availableRows: this.props.result.rows,
      availableChunks: 1,
      loadingChunks: false,
      loadError: false,
    };

    this.componentDidUpdate = this.componentDidUpdate.bind(this);

    this.handleClickMoreRows = this.handleClickMoreRows.bind(this);
    this.handleClickLessRows = this.handleClickLessRows.bind(this);

    this.loadAndShowMoreRows = this.loadAndShowMoreRows.bind(this);
    this.renderRows = this.renderRows.bind(this);
    this.renderShowAll = this.renderShowAll.bind(this);
    this.renderChartButton = this.renderChartButton.bind(this);
    this.renderAxesButton = this.renderAxesButton.bind(this);
    this.renderOptionMenu = this.renderOptionMenu.bind(this);

    this.conditionallyRenderChart = this.conditionallyRenderChart.bind(this);
    this.conditionallyRenderChartConfig = this.conditionallyRenderChartConfig.bind(this);
    this.formatValue = this.formatValue.bind(this);

    this.showingAllOfFewRows = this.showingAllOfFewRows.bind(this);
    this.showingAllOfManyRows = this.showingAllOfManyRows.bind(this);
    this.showingMinimumNumberOfManyRows = this.showingMinimumNumberOfManyRows.bind(this);

    this.graphInfo = new GraphInfo(this.props.result.columns, this.state.availableRows);
    this.rebuildGraphData();

    this.addX = this.addX.bind(this);
    this.addY = this.addY.bind(this);
    this.removeColumn = this.removeColumn.bind(this);

    this.getInfoMessages = this.getInfoMessages.bind(this);
  }

  minRowsToShow: number;

  graphInfo: GraphInfoT;

  graphData: GraphDataT;

  static contextType = AuthContext;

  componentDidUpdate = () => {
    this.rebuildGraphData();
  }

  rebuildGraphData = () => {
    this.graphData = new GraphData(
      this.props.result.columns,
      this.state.availableRows,
      this.state.graphConfig,
      this.formatValue,
    );
  }

  handleClickMoreRows = () => {
    const rowsToShowCount = Math.min(2 * this.state.rowsToShowCount, this.props.result.row_count);
    this.loadAndShowMoreRows(rowsToShowCount, this.state.availableRows, this.state.availableChunks);
  }

  handleClickLessRows = () => {
    const rowsToShowCount = Math.max(Math.round(this.state.rowsToShowCount / 2), this.minRowsToShow);
    this.setState({rowsToShowCount});
  }

  showChart = () => {
    if (this.state.availableChunks !== ALL_CHUNKS) {
      this.loadChunks(ALL_CHUNKS, (allRows) => {
        this.setState({availableRows: allRows, availableChunks: ALL_CHUNKS, showChart: true});
      });
    } else {
      this.setState({showChart: true});
    }
  }

  loadAndShowMoreRows = (rowsToShowCount: number, availableRows: Row[], availableChunks: number) => {
    const availableRowsCount = _.sum(_.flatMap(availableRows, (row) => row.occurrences));
    if (availableChunks === ALL_CHUNKS || rowsToShowCount <= availableRowsCount) {
      this.setState({rowsToShowCount, availableRows, availableChunks});
    } else {
      this.loadChunks(availableChunks, (newRows) => {
        if (newRows.length > 0) {
          const newAvailableRows = _.concat(availableRows, newRows);
          const newAvailableChunks = availableChunks + 1;
          this.setState({
            rowsToShowCount: availableRowsCount,
            availableRows: newAvailableRows,
            availableChunks: newAvailableChunks,
          });
          this.loadAndShowMoreRows(rowsToShowCount, newAvailableRows, newAvailableChunks);
        }
      });
    }
  }

  loadChunks = (desiredChunk: number, fun: ((rows: Row[]) => void)) => {
    this.setState({loadingChunks: true, loadError: false});
    loadBuckets(this.props.result.buckets_link, desiredChunk, this.context.authentication, {
      success: (buckets) => {
        this.setState({loadingChunks: false, loadError: false});
        fun(buckets);
      },
      error: () => {
        this.setState({loadingChunks: false, loadError: true});
      },
    });
  }

  showingAllOfFewRows = () => this.props.result.row_count <= this.minRowsToShow

  showingAllOfManyRows = () => this.props.result.row_count === this.state.rowsToShowCount

  showingMinimumNumberOfManyRows = () => this.state.rowsToShowCount === this.minRowsToShow && this.props.result.row_count > this.minRowsToShow

  addX = (col: number) => () => this.setState({graphConfig: this.state.graphConfig.addX(col)})

  addY = (col: number) => () => this.setState({graphConfig: this.state.graphConfig.addY(col)})

  removeColumn = (col: number) => () => this.setState({graphConfig: this.state.graphConfig.remove(col)})

  formatValue = (value: any, columnIndex: number): string => {
    const type = this.props.result.types[columnIndex];
    if (value === null) {
      return "<null>";
    } else if (value === "") {
      return ZERO_WIDTH_SPACE; // keeps table row from collapsing
    } else if (this.isNumeric(value)) {
      return formatNumber(value, this.props.numberFormat);
    } else if (type === "datetime") {
      return this.formatDateTime(value);
    } else if (type === "time") {
      return this.formatTime(value);
    } else {
      return value.toString();
    }
  }

  isNumeric = (n: any): boolean => typeof (n) === "number" && isFinite(n)

  formatDateTime = (value: string): string => {
    const [date, time] = value.split("T");
    if (time === undefined) {
      return date;
    } else {
      return `${date} ${this.formatTime(time)}`;
    }
  }

  formatTime = (value: string): string => {
    const [hms, us] = value.split(".");
    const ZERO_US = "000000";
    if (us === ZERO_US || us === undefined) {
      return hms;
    } else {
      const SUBSECOND_PRECISION = 3;
      return `${hms}.${us.substr(0, SUBSECOND_PRECISION)}`;
    }
  }

  conditionallyRenderChart = () => {
    if (this.state.showChart) {
      return (
        <GraphView
          graphData={this.graphData}
          width={714}
          height={600}
        />
      );
    } else {
      return null;
    }
  }

  conditionallyRenderChartConfig = () => {
    if (this.state.loadingChunks) {
      return (
        <p className="text-center">
          {" "}
          <img src="/images/loader.gif" role="presentation" />
          {" "}
          Loading more rows.
        </p>
      );
    } else if (this.state.loadError) {
      return (<div className="alert alert-danger">Failed to load more rows.</div>);
    } else if (this.state.showChart && this.state.showChartConfig) {
      return (
        <GraphConfigView
          graphInfo={this.graphInfo}
          graphConfig={this.state.graphConfig}
          addX={this.addX}
          addY={this.addY}
          remove={this.removeColumn}
        />
      );
    } else {
      return null;
    }
  }

  getRowAttrs = (row: Row) => {
    if (row.unreliable) {
      return {
        title: "These values are unreliable because of the low number of users involved.",
        "data-toggle": "tooltip",
        className: "unreliable",
      };
    } else {
      return {};
    }
  }

  getInfoMessages = (): string[] => {
    const messages = this.props.result.info;
    if (!this.props.debugModeEnabled) {
      return messages.filter((message) => !message.startsWith("[Debug]"));
    } else {
      return messages;
    }
  }

  renderRows = () => {
    let remainingRowsToProduce = this.state.rowsToShowCount;
    const rows = _.flatMap(this.state.availableRows, (accumulateRow, i) => {
      const occurrencesForAccumulateRow = Math.min(remainingRowsToProduce, accumulateRow.occurrences);
      return _.range(occurrencesForAccumulateRow).map((occurrenceCount) => {
        remainingRowsToProduce -= 1;
        return (
          <tr key={`${i}-${occurrenceCount}`} {...this.getRowAttrs(accumulateRow)}>
            {accumulateRow.row.map((value, j) => (
              <td key={j} className={this.state.tableAligner.alignmentClass(j)}>
                {this.formatValue(value, j)}
              </td>
            ))}
          </tr>
        );
      });
    });
    activateTooltips();
    return rows;
  }

  renderShowAll = () => {
    if (this.state.loadingChunks) {
      return null;
    } else if (this.showingAllOfFewRows()) {
      return (
        <div className="row-count">
          {this.props.result.row_count}
          {" "}
rows.
        </div>
      );
    } else if (this.showingAllOfManyRows()) {
      return (
        <div className="row-count">
          {this.props.result.row_count}
          {" "}
rows.&nbsp;
          <a onClick={this.handleClickLessRows}>Show fewer rows</a>
        </div>
      );
    } else if (this.showingMinimumNumberOfManyRows()) {
      return (
        <div className="row-count">
          Showing
          {" "}
          {this.minRowsToShow}
          {" of "}
          {this.props.result.row_count}
          {" rows "}
          <a onClick={this.handleClickMoreRows}>Show more rows</a>
        </div>
      );
    } else {
      const rowsShown = Math.min(this.state.rowsToShowCount, this.props.result.row_count);
      return (
        <div className="row-count">
          Showing
          {" "}
          {rowsShown}
          {" "}
of
          {" "}
          {this.props.result.row_count}
          {" rows. Show "}
          <a onClick={this.handleClickLessRows}>fewer rows</a>
          {", "}
          <a onClick={this.handleClickMoreRows}>more rows</a>
        </div>
      );
    }
  }

  renderChartButton = () => {
    if (this.graphInfo.chartable()) {
      const chartButtonText = this.state.showChart ? "Hide chart" : "Show chart";
      return (
        <button
          className={this.chartButtonClass()}
          onClick={() => {
            if (!this.state.loadingChunks) {
              const shouldShowChart = !this.state.showChart;
              if (shouldShowChart) {
                this.showChart();
              } else {
                this.setState({showChart: shouldShowChart});
              }
            }
          }}
        >
          {chartButtonText}
        </button>
      );
    } else {
      return null;
    }
  }

  chartButtonClass = () => {
    const baseClasses = "btn btn-default btn-xs";
    if (this.state.loadingChunks) {
      return `${baseClasses} disabled`;
    } else {
      return baseClasses;
    }
  }

  renderAxesButton = () => {
    if (this.state.showChart) {
      const text = this.state.showChartConfig ? "Hide axes" : "Show axes";
      return (
        <button
          className="btn btn-default btn-xs"
          onClick={() => this.setState({showChartConfig: !this.state.showChartConfig})}
        >
          {text}
        </button>
      );
    } else {
      return null;
    }
  }

  renderOptionMenu = () => (
    <div className="options-menu">
      <ShareButton result={this.props.result} />
      <a className="btn btn-default btn-xs" href={`/queries/${this.props.result.id}.csv`}>Download as CSV</a>
      <DebugExport id={this.props.result.id} debugModeEnabled={this.props.debugModeEnabled} />
      {this.renderChartButton()}
      {this.renderAxesButton()}
    </div>
  )

  render = () => (
    <div className="panel panel-success">
      <div className="panel-heading" />
      <div className="panel-body">
        <CodeViewer statement={this.props.result.statement} />
        <Info info={this.getInfoMessages()} />
        <div className="result-table">
          <table className="table table-striped table-condensed table-hover">
            <thead>
              <tr>
                {this.props.result.columns.map((column, i) => <th key={i} className={this.state.tableAligner.alignmentClass(i)}>{column}</th>)}
              </tr>
            </thead>

            <tbody>
              {this.renderRows()}
            </tbody>
          </table>
        </div>
        {this.renderShowAll()}
        {this.renderOptionMenu()}
        {this.conditionallyRenderChartConfig()}
        {this.conditionallyRenderChart()}
      </div>
    </div>
  )
}
