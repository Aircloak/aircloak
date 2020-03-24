import $ from "jquery";
import moment from "moment";
import "tempusdominus-bootstrap-4/build/css/tempusdominus-bootstrap-4.css";

// Unfortunately there do not seem to be many datetime pickers that
// follow modern JS best-practice. Hence the following hack.
const activate = () => {
  window.jQuery = $;
  window.moment = moment;
  const promise = import("tempusdominus-bootstrap-4");
  promise.then(() => {
    $.fn.datetimepicker.Constructor.Default = $.extend(
      {},
      $.fn.datetimepicker.Constructor.Default,
      {
        icons: {
          time: "fa fa-clock",
          date: "fa fa-calendar-alt",
          up: "fa fa-arrow-up",
          down: "fa fa-arrow-down",
          previous: "fa fa-chevron-left",
          next: "fa fa-chevron-right",
          today: "fa fa-calendar-check",
          clear: "fa fa-trash",
          close: "fa fa-times"
        },
        allowInputToggle: true,
        format: "YYYY-MM-DD HH:mm:ss",
        useCurrent: false,
        buttons: {
          showToday: true,
          showClose: true,
          showClear: false
        }
      }
    );
    delete window.jQuery;
    delete window.moment;
  });
  return async () => {
    await promise;
    $(".datetimepicker-input").datetimepicker({
      format: "YYYY-MM-DD HH:mm:ss"
    });
  };
};

export default activate();
