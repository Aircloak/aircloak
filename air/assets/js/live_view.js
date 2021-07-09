import { Socket } from "phoenix";
import LiveSocket from "phoenix_live_view";
import CodeMirror from "codemirror";
import NProgress from "nprogress";
import "nprogress/nprogress.css";
import "./elements/histogram_chart";
import "./elements/sql_code_block";
import activateDatetimePickers from "./datetimepicker";
import FrontendSocket from "./frontend_socket";

export default (transportName) => {
  const csrfElement = document.querySelector("meta[name='csrf-token']");
  const _csrf_token = csrfElement && csrfElement.getAttribute("content");
  window.addEventListener("phx:page-loading-start", (info) =>
    NProgress.start()
  );
  window.addEventListener("phx:page-loading-stop", (info) => NProgress.done());
  const liveSocket = new LiveSocket("/live", Socket, {
    timeout: 30000,
    params: { _csrf_token },
    transport: FrontendSocket.transport(transportName),
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
      DatePickerHook: {
        mounted() {
          activateDatetimePickers();
        },
      },
      ...(window.liveHooks || {}),
    },
  });
  liveSocket.connect();
  window.live = liveSocket;
};
