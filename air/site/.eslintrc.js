module.exports = {
  "extends": "airbnb",
  "plugins": [
    "react"
  ],
  "rules": {
    "allow-unused-underscore-vars": ["error"],
    "no-else-return": ["off"],
    "quotes": ["error", "double"],
    "object-curly-spacing": ["error", "never"],
    "max-len": ["error", {"code": 110}],
    /* FIXME break out all components to separate files and turn this on */
    "react/no-multi-comp": ["off"],
  }
};
