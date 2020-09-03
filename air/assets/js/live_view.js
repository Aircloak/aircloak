import { Socket } from "phoenix";
import LiveSocket from "phoenix_live_view";
import CodeMirror from "codemirror";

export default () => {
  const csrfElement = document.querySelector("meta[name='csrf-token']");
  const _csrf_token = csrfElement && csrfElement.getAttribute("content");
  const liveSocket = new LiveSocket("/live", Socket, {
    params: { _csrf_token },
    hooks: {
      SQLCodeViewer: {
        mounted() {
          const cm = CodeMirror(
            (elt) => {
              this.el.parentNode.replaceChild(elt, this.el);
              elt.style.fontSize = "14px";
            },
            {
              value: this.el.textContent,
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
          const { parentElement, clientHeight } = cm.getWrapperElement();

          if (clientHeight > 60) {
            parentElement.classList.add("expander", "collapsed");
            parentElement.style.setProperty(
              "--full-height",
              clientHeight + "px"
            );
            const listener = () => {
              parentElement.classList.toggle("collapsed");
              parentElement.removeEventListener("click", listener);
            };
            parentElement.addEventListener("click", listener);
          }
        },
      },
    },
  });
  liveSocket.connect();
  window.live = liveSocket;
};
