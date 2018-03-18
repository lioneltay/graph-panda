var R = require("ramda");
var formatFilters = require("./formatFilters").formatFilters;
var formattedFiltersToWhereAST = require("./formattedFiltersToWhereAST").formattedFiltersToWhereAST;
var astToSqlWhere = require("./astToSqlWhere").astToSqlWhere;
var formatSql = require("sql-formatter").format;
var generateWhereClause = function (filters, _a) {
    var _b = _a === void 0 ? {} : _a, type = _b.type, schema = _b.schema, tableName = _b.tableName, _c = _b.debug, debug = _c === void 0 ? false : _c;
    if (debug) {
        console.log({ type: type, tableName: tableName });
        console.log(JSON.stringify(filters, null, 2));
    }
    if (R.isNil(filters) || R.isEmpty(filters)) {
        return undefined;
    }
    if (!type || !schema || !tableName) {
        throw Error("generateWhereClause: Must provide type, schema and tableName");
    }
    // console.log("BEGIN")
    // Format the filters to use a more verbose syntax
    // No domain logic here
    var formattedFilters = formatFilters(filters, { type: type, schema: schema });
    if (debug) {
        console.log('===== FORMATTED FILTERS =====');
        console.log(JSON.stringify(formattedFilters, null, 2));
    }
    // console.log('FORMATTED FILTERS\n', JSON.stringify(formattedFilters, null, 2))
    // Turn the filters into an sql where AST
    // ALL the domain logic is here
    // The sql stuff happens here
    var whereAST = formattedFiltersToWhereAST(formattedFilters, {
        type: type,
        tableName: tableName,
        schema: schema,
    });
    if (debug) {
        console.log('===== WHERE AST =====');
        console.log(JSON.stringify(whereAST, null, 2));
    }
    // console.log('whereAST \n', JSON.stringify(whereAST, null, 2))
    // Turn the AST into a where clause string
    // No domain logic here
    var sql = astToSqlWhere(whereAST);
    if (debug) {
        console.log('===== SQL =====');
        console.log(JSON.stringify(sql, null, 2));
    }
    // console.log('sql \n', JSON.stringify(sql, null, 2))
    return formatSql(sql);
};
module.exports = {
    generateWhereClause: generateWhereClause,
};
setTimeout(function () {
    console.log("\n+++ GRAPHQL FILTER TEST +++\n");
    console.log("hello");
    console.log("\n+++++++++++++++++++++++++++");
}, 1000);
//# sourceMappingURL=index.js.map