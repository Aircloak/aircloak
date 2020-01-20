// @flow

import React from "react";
import _ from "lodash";

import {AuthContext} from "../authentication_provider";
import CodeViewer from "../code_viewer";
import InfoView from "./info_view";
import {GraphData, GraphInfo, GraphConfig} from "./graph_data";
import {GraphConfigView} from "./graph_config_view";
import {GraphView} from "./graph_view";
import type {GraphDataT, GraphInfoT} from "./graph_data";
import {TableAligner} from "./table_aligner";
import type {TableAlignerT} from "./table_aligner";
import type {NumberFormat} from "../number_format";
import {formatNumber} from "../number_format";
import {loadBuckets} from "../request";
import DebugExport from "./debug_export";
import ShareButton from "./share_button";
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
    const {result} = props;

    this.state = {
      rowsToShowCount: this.minRowsToShow,
      showChart: false,
      showChartConfig: true,
      graphConfig: new GraphConfig(),
      tableAligner: new TableAligner(result.rows),
      availableRows: result.rows,
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

    const {availableRows} = this.state;
    this.graphInfo = new GraphInfo(result.columns, availableRows);
    this.rebuildGraphData();

    this.addX = this.addX.bind(this);
    this.addY = this.addY.bind(this);
    this.removeColumn = this.removeColumn.bind(this);

    this.getInfoMessages = this.getInfoMessages.bind(this);
  }

  minRowsToShow: number;

  graphInfo: GraphInfoT;

  graphData: GraphDataT;

  componentDidUpdate = () => {
    this.rebuildGraphData();
  }

  rebuildGraphData = () => {
    const {result} = this.props;
    const {availableRows, graphConfig} = this.state;
    this.graphData = new GraphData(
      result.columns,
      availableRows,
      graphConfig,
      this.formatValue,
    );
  }

  handleClickMoreRows = () => {
    const {result} = this.props;
    const {rowsToShowCount, availableRows, availableChunks} = this.state;
    const updatedRowsToShowCount = Math.min(2 * rowsToShowCount, result.row_count);
    this.loadAndShowMoreRows(updatedRowsToShowCount, availableRows, availableChunks);
  }

  handleClickLessRows = () => {
    const {rowsToShowCount} = this.state;
    const updatedRowsToShowCount = Math.max(Math.round(rowsToShowCount / 2), this.minRowsToShow);
    this.setState({rowsToShowCount: updatedRowsToShowCount});
  }

  showChart = () => {
    const {availableChunks} = this.state;
    if (availableChunks !== ALL_CHUNKS) {
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
    const {result} = this.props;
    const {authentication} = this.context;
    loadBuckets(result.buckets_link, desiredChunk, authentication, {
      success: (buckets) => {
        this.setState({loadingChunks: false, loadError: false});
        fun(buckets);
      },
      error: () => {
        this.setState({loadingChunks: false, loadError: true});
      },
    });
  }

  showingAllOfFewRows = () => {
    const {result} = this.props;
    return result.row_count <= this.minRowsToShow;
  }

  showingAllOfManyRows = () => {
    const {result} = this.props;
    const {rowsToShowCount} = this.state;
    return result.row_count === rowsToShowCount;
  }

  showingMinimumNumberOfManyRows = () => {
    const {result} = this.props;
    const {rowsToShowCount} = this.state;
    return rowsToShowCount === this.minRowsToShow && result.row_count > this.minRowsToShow;
  }

  addX = (col: number) => () => {
    const {graphConfig} = this.state;
    this.setState({graphConfig: graphConfig.addX(col)});
  }

  addY = (col: number) => () => {
    const {graphConfig} = this.state;
    this.setState({graphConfig: graphConfig.addY(col)});
  }

  removeColumn = (col: number) => () => {
    const {graphConfig} = this.state;
    this.setState({graphConfig: graphConfig.remove(col)});
  }

  formatValue = (value: any, columnIndex: number): string => {
    const {result, numberFormat} = this.props;
    const type = result.types[columnIndex];
    if (value === null) {
      return "<null>";
    } else if (value === "") {
      return ZERO_WIDTH_SPACE; // keeps table row from collapsing
    } else if (this.isNumeric(value)) {
      return formatNumber(value, numberFormat);
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
    const {showChart} = this.state;
    if (showChart) {
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
    const {
      loadingChunks, loadError, showChart, showChartConfig, graphConfig,
    } = this.state;
    if (loadingChunks) {
      return (
        <p className="text-center">
          {" "}
          <img alt="indication of more rows being loaded" src="/images/loader.gif" />
          {" "}
          Loading more rows.
        </p>
      );
    } else if (loadError) {
      return (<div className="alert alert-danger">Failed to load more rows.</div>);
    } else if (showChart && showChartConfig) {
      return (
        <GraphConfigView
          graphInfo={this.graphInfo}
          graphConfig={graphConfig}
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
    const {result, debugModeEnabled} = this.props;
    const messages = result.info;
    if (!debugModeEnabled) {
      return messages.filter((message) => !message.startsWith("[Debug]"));
    } else {
      return messages;
    }
  }

  renderRows = () => {
    const {rowsToShowCount, availableRows, tableAligner} = this.state;
    let remainingRowsToProduce = rowsToShowCount;
    const rows = _.flatMap(availableRows, (accumulateRow, i) => {
      const occurrencesForAccumulateRow = Math.min(remainingRowsToProduce, accumulateRow.occurrences);
      return _.range(occurrencesForAccumulateRow).map((occurrenceCount) => {
        remainingRowsToProduce -= 1;
        return (
          <tr key={`${i}-${occurrenceCount}`} {...this.getRowAttrs(accumulateRow)}>
            {accumulateRow.row.map((value, j) => (
              <td key={j} className={tableAligner.alignmentClass(j)}>
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
    const {result} = this.props;
    const {loadingChunks, rowsToShowCount} = this.state;
    if (loadingChunks) {
      return null;
    } else if (this.showingAllOfFewRows()) {
      return (
        <div className="row-count">
          {result.row_count}
          {" "}
rows.
        </div>
      );
    } else if (this.showingAllOfManyRows()) {
      return (
        <div className="row-count">
          {result.row_count}
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
          {result.row_count}
          {" rows "}
          <a onClick={this.handleClickMoreRows}>Show more rows</a>
        </div>
      );
    } else {
      const rowsShown = Math.min(rowsToShowCount, result.row_count);
      return (
        <div className="row-count">
          Showing
          {" "}
          {rowsShown}
          {" "}
of
          {" "}
          {result.row_count}
          {" rows. Show "}
          <a onClick={this.handleClickLessRows}>fewer rows</a>
          {", "}
          <a onClick={this.handleClickMoreRows}>more rows</a>
        </div>
      );
    }
  }

  renderChartButton = () => {
    const {showChart, loadingChunks} = this.state;
    if (this.graphInfo.chartable()) {
      const chartButtonText = showChart ? "Hide chart" : "Show chart";
      return (
        <button
          type="button"
          className={this.chartButtonClass()}
          onClick={() => {
            if (!loadingChunks) {
              const shouldShowChart = !showChart;
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
    const {loadingChunks} = this.state;
    const baseClasses = "btn btn-default btn-xs";
    if (loadingChunks) {
      return `${baseClasses} disabled`;
    } else {
      return baseClasses;
    }
  }

  renderAxesButton = () => {
    const {showChart, showChartConfig} = this.state;
    if (showChart) {
      const text = showChartConfig ? "Hide axes" : "Show axes";
      return (
        <button
          type="button"
          className="btn btn-default btn-xs"
          onClick={() => this.setState({showChartConfig: !showChartConfig})}
        >
          {text}
        </button>
      );
    } else {
      return null;
    }
  }

  renderOptionMenu = () => {
    const {result, debugModeEnabled} = this.props;
    return (
      <div className="options-menu">
        <ShareButton result={result} />
        <a className="btn btn-default btn-xs" href={`/queries/${result.id}.csv`}>Download as CSV</a>
        <DebugExport id={result.id} debugModeEnabled={debugModeEnabled} />
        {this.renderChartButton()}
        {this.renderAxesButton()}
      </div>
    );
  }

  render = () => {
    const {result} = this.props;
    const {tableAligner} = this.state;
    return (
      <div className="panel panel-success">
        <div className="panel-heading" />
        <div className="panel-body">
          <CodeViewer statement={result.statement} />
          <InfoView info={this.getInfoMessages()} />
          <div className="result-table">
            <table className="table table-striped table-condensed table-hover">
              <thead>
                <tr>
                  {result.columns.map((column, i) => <th key={i} className={tableAligner.alignmentClass(i)}>{column}</th>)}
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
    );
  }
}

ResultView.contextType = AuthContext;
