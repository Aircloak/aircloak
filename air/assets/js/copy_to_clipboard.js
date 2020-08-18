// @flow

export default (targetId: string) => {
  let range = document.createRange();
  let target = document.getElementById(targetId);

  if (target) {
    range.selectNode(target);
    window.getSelection().removeAllRanges();
    window.getSelection().addRange(range);
    document.execCommand("copy");
    window.getSelection().removeAllRanges();
  }
};
