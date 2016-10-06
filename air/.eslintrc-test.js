module.exports = {
  "extends": "airbnb",
  "rules": {
    "allow-unused-underscore-vars": ["error"],
    "no-else-return": ["off"],
    "quotes": ["error", "double"],
    "object-curly-spacing": ["error", "never"],
    "max-len": ["error", {"code": 110}],
    "import/no-unresolved": ["off"],
    "flowtype/require-valid-file-annotation": ["off"],
  },
  "env": {
    "node": true,
    "mocha": true,
    "es6": true,
  }
};
