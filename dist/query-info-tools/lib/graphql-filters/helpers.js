var R = require("ramda");
var splitSingleProperty = function (object) {
    var key = R.head(R.keys(object));
    var value = object[key];
    return { key: key, value: value };
};
var singleKey = function (object) { return R.keys(object).length === 1; };
var onlyKey = function (object) {
    if (R.keys(object).length !== 1) {
        throw Error("onlyKey used on object with keys.length (" + R.keys(object).length + ") !== 1\n        " + JSON.stringify(object, null, 2) + "\n      ");
    }
    return splitSingleProperty(object).key;
};
var onlyValue = function (object) {
    if (R.values(object).length !== 1) {
        throw Error("onlyValue used on object with values.length !== 1");
    }
    return splitSingleProperty(object).value;
};
var objToArray = R.pipe(R.toPairs, R.map(function (_a) {
    var k = _a[0], v = _a[1];
    return (_b = {}, _b[k] = v, _b);
    var _b;
}));
module.exports = {
    splitSingleProperty: splitSingleProperty,
    onlyKey: onlyKey,
    onlyValue: onlyValue,
    singleKey: singleKey,
    objToArray: objToArray,
};
//# sourceMappingURL=helpers.js.map