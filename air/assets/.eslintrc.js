module.exports = {
  "extends": [
    "airbnb",
    "plugin:flowtype/recommended",
    "plugin:import/errors",
    "plugin:import/warnings",
  ],
  "parser": "babel-eslint",
  "plugins": [
    "react",
    "flowtype"
  ],
  "rules": {
    "allow-unused-underscore-vars": ["error"],
    "no-else-return": ["off"],
    "quotes": ["error", "double"],
    "object-curly-spacing": ["error", "never"],
    "max-len": ["error", {"code": 120}],
    /* FIXME break out all components to separate files and turn this on */
    "react/no-multi-comp": ["off"],
    "flowtype/require-valid-file-annotation": [2, "always"],
    /* Does not play well with flow annotations */
    "react/sort-comp": ["off"],
    /* Handled by plugin-import */
    "no-duplicate-imports": ["off"],
    "no-underscore-dangle": ["off"],
    "react/jsx-filename-extension": [0],
  },
  "settings": {
    "flowtype": {
      "onlyFilesWithFlowAnnotation": false
    }
  },
  "globals": {
    "window": true,
    "confirm": true
  }
};
