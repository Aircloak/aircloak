import _ from "lodash";

export class GraphData {
  constructor(rows, columns) {
    this.rows = rows;
    this.columns = columns;

    this.yColumns = this.yColumns.bind(this);
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

  isNumeric(n) {
    return typeof(n) === "number" && isFinite(n);
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
}


