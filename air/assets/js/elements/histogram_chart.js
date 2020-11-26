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
      this.data = JSON.parse(this.getAttribute("data"));
      const mobile = window.innerWidth < 1000;
      const embed = vegaEmbed(
        container,
        {
          datasets: {
            data: this.data,
          },
          [mobile ? "vconcat" : "hconcat"]: [
            {
              width:
                window.innerWidth < 600
                  ? window.innerWidth - 170
                  : window.innerWidth < 1000
                  ? window.innerWidth - 400
                  : Math.min(window.innerWidth - 900, 600),
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
                  axis: { title: "Total runtime (sec; binned)" },
                },
                x2: {
                  field: "max",
                  type: "quantitative",
                },
                y: {
                  field: "count",
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
                tooltip: true,
              },
              data: {
                name: "data",
              },
              transform: [
                { filter: { selection: "selectedBucket" } },
                {
                  fold: [
                    "started",
                    "parsing",
                    "compiling",
                    "awaiting_data",
                    "ingesting_data",
                    "processing",
                    "post_processing",
                    "completed",
                  ],
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
              ],
              encoding: {
                x: {
                  field: "avg",
                  type: "quantitative",
                  axis: { title: "Runtime by execution phase (sec; avg)" },
                },
                color: {
                  field: "key",
                  legend: {
                    orient: "bottom",
                    direction:
                      window.innerWidth > 1300 ? "horizontal" : "vertical",
                  },
                  title: "Execution Phase",
                },
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
        this.view && this.view.data("data", this.data).runAsync();
      }
    }

    static get observedAttributes() {
      return ["data"];
    }
  }
);
