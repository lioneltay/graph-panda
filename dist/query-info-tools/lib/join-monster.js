var joinMonster = require("join-monster").default;
var R = require("ramda");
var knex = require.main.require("../common/functions/knex");
var formatSql = require("sql-formatter").format;
var generateWhereClause = require.main.require("../lib/graphql-filters").generateWhereClause;
var debug = true;
var defaultDBCall = function (sql) { return knex.raw(sql).then(R.prop("rows")); };
var generateContext = function () { return ({
    knex: knex,
    schema: require.main.require("../graphql-schema"),
    formatSql: formatSql,
    generateWhereClause: function (filters, options) {
        return generateWhereClause(filters, R.merge(options, {
            type: options.type,
            tableName: options.tableName,
            schema: require.main.require("../graphql-schema"),
        }));
    },
}); };
var jm = function (resolveInfo, context, dbCall, options) {
    if (context === void 0) { context = {}; }
    if (dbCall === void 0) { dbCall = defaultDBCall; }
    return joinMonster(resolveInfo, R.merge(generateContext(), context), function (sql) {
        var result = dbCall(sql);
        if (debug) {
            console.log("\n======================");
            console.log(sql);
            console.log("\n======================");
            result.then(function (rows) {
                console.log("Join-Monster Query rows: " + rows.length);
                // console.log(rows)
            });
        }
        return result;
    }, R.merge({ dialect: "pg" }, options));
};
jm.infoOnly = function (root, args, context, info) { return jm(info); };
module.exports = jm;
//# sourceMappingURL=join-monster.js.map