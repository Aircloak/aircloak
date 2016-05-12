const STARTS_WITH_UNDERSCORE = /^_./;

module.exports = function(context) {
  return {
    "Identifier": function(node) {
      if (node.name.match(STARTS_WITH_UNDERSCORE)) {
        context.markVariableAsUsed(node.name);
      };
    },
  };
};

module.exports.schema = [];
