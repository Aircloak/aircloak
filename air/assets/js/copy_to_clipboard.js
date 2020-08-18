// @flow

export default (targetId: string) => {
  const range = document.createRange();
  const target = document.getElementById(targetId);

  if (target) {
    range.selectNode(target);
    window.getSelection().removeAllRanges();
    window.getSelection().addRange(range);
    document.execCommand("copy");
    window.getSelection().removeAllRanges();
  }
};
