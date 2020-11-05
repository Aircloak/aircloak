// @flow

export default (targetId: string) => {
  const target = document.getElementById(targetId);

  if (target && target instanceof HTMLInputElement) {
    target.focus();
    target.select();
    document.execCommand("copy");
  }
};
