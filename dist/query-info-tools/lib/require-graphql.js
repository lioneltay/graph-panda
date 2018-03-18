var path = require("path");
var fs = require("fs");
var R = require("ramda");
module.exports = R.curry(function (module, filePath) {
    var initPath = R.init(module.filename.split("/")).join("/");
    var wholePath = path.resolve(initPath, filePath);
    var completePath = wholePath + ".graphql";
    return fs.readFileSync(completePath, "utf8");
});
//# sourceMappingURL=require-graphql.js.map