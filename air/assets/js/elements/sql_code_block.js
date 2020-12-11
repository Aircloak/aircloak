import CodeMirror from "codemirror";

customElements.define(
  "sql-code-block",
  class extends HTMLElement {
    constructor() {
      super();

      this.cm = CodeMirror(
        (elt) => {
          this.appendChild(elt, this);
          elt.style.fontSize = "14px";
        },
        {
          value: this.getAttribute("code"),
          indentUnit: 2,
          indentWithTabs: false,
          lineNumbers: false,
          lineWrapping: true,
          readOnly: true,
          mode: "text/x-aircloak-sql",
          showCursorWhenSelecting: true,
          smartIndent: true,
          viewportMargin: Infinity,
          cursorBlinkRate: -1,
        }
      );
    }

    attributeChangedCallback(name, oldValue, newValue) {
      if (name === "code") {
        this.cm.setValue(newValue);
      }
    }

    static get observedAttributes() {
      return ["code"];
    }
  }
);
