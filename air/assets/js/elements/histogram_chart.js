import vegaEmbed from "vega-embed";

customElements.define(
  "histogram-chart",
  class extends HTMLElement {
    constructor() {
      super();
      const shadowRoot = this.attachShadow({ mode: "open" });
      const container = document.createElement("div");
      container.style.width = "100%";
      container.style.height = "100%";
      shadowRoot.appendChild(container);
      const availableWidth = container.clientWidth;

      this.data = JSON.parse(this.getAttribute("data"));
      const mobile = availableWidth < 500;
      const responsive = (ratio) =>
        mobile ? availableWidth - 45 : availableWidth * ratio - 45;

      const phases = [
        "started",
        "parsing",
        "compiling",
        "waiting for database",
        "ingesting data",
        "processing",
        "post-processing",
        "completed",
      ];
      const embed = vegaEmbed(
        container,
        {
          datasets: {
            data: this.data,
          },
          resolve: {
            legend: {
              color: "independent",
            },
          },
          [mobile ? "vconcat" : "hconcat"]: [
            {
              width: responsive(0.6),
              mark: {
                type: "bar",
                tooltip: true,
              },
              data: {
                name: "data",
              },
              selection: {
                selectedBucket: { type: "single", on: "mouseover" },
              },
              encoding: {
                x: {
                  field: "min",
                  bin: "binned",
                  type: "quantitative",
                  title: "Total runtime",
                  format: ".2f",
                  axis: { title: "Total runtime (sec; binned)" },
                },
                x2: {
                  field: "max",
                  type: "quantitative",
                },
                y: {
                  field: "count",
                  title: "Count",
                  type: "quantitative",
                  scale: { type: "symlog" },
                  axis: { title: "Count (log scale)" },
                },
                fill: { value: "#1eb9d6" },
                fillOpacity: {
                  condition: {
                    selection: "selectedBucket",
                    value: 1,
                  },
                  value: 0.6,
                },
              },
            },
            {
              mark: {
                type: "bar",
              },
              width: responsive(0.4),
              data: {
                name: "data",
              },
              transform: [
                { filter: { selection: "selectedBucket" } },
                {
                  fold: phases,
                },
                {
                  aggregate: [
                    {
                      op: "average",
                      field: "value",
                      as: "avg",
                    },
                  ],
                  groupby: ["key"],
                },
                {
                  window: [
                    {
                      op: "sum",
                      field: "avg",
                      as: "total_time",
                    },
                  ],
                  frame: [null, null],
                },
                {
                  calculate: "datum.avg/datum.total_time",
                  as: "percent_of_total",
                },
                {
                  calculate: `indexof(${JSON.stringify(phases)}, datum.key)`,
                  as: "sort_index",
                },
              ],

              encoding: {
                x: {
                  field: "avg",
                  type: "quantitative",
                  axis: { title: "Runtime by execution phase (sec; avg)" },
                },
                order: {
                  field: "sort_index",
                },
                color: {
                  field: "key",
                  legend: {
                    orient: "bottom",
                    direction: "vertical",
                    values: phases,
                  },
                  title: "Execution Phase",
                },
                tooltip: [
                  {
                    field: "key",
                    title: "Execution Phase",
                  },
                  {
                    field: "avg",
                    type: "quantitative",
                    format: ".2f",
                    title: "Runtime",
                  },
                  {
                    field: "percent_of_total",
                    type: "quantitative",
                    format: ".1~%",
                    title: "Of total",
                  },
                ],
              },
            },
          ],
        },
        { actions: false }
      );

      embed.then((e) => {
        this.view = e.view;
      });
    }

    attributeChangedCallback(name, oldValue, newValue) {
      if (name === "data") {
        this.data = JSON.parse(newValue);

        if (this.view)
          this.view
            .data("selectedBucket_store", [])
            .data("data", this.data)
            .runAsync();
      }
    }

    static get observedAttributes() {
      return ["data"];
    }
  }
);
