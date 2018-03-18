var R = require("ramda");
var sqlFormatter = require("sql-formatter");
var knex = require("../knex");
// Turn filter AST into where string
var astToSqlWhere = function (ast) {
    var astToSql_ = function (ast) {
        if (ast.NOT) {
            var subSql = astToSql_(ast.NOT);
            return "(NOT " + subSql + ")";
        }
        if (ast.AND) {
            var subSql = ast.AND.map(function (subAst) { return astToSql_(subAst); }).join(" AND ");
            return "(" + subSql + ")";
        }
        if (ast.OR) {
            var subSql = ast.OR.map(function (subAst) { return astToSql_(subAst); }).join(" OR ");
            return "(" + subSql + ")";
        }
        if (ast.joins) {
            var subSql = astToSql_(ast.subClause);
            return "" + ast.joins(subSql);
        }
        return knex.raw(ast.clause, ast.variables).toString();
    };
    return sqlFormatter.format(astToSql_(ast));
};
module.exports = {
    astToSqlWhere: astToSqlWhere,
};
//# sourceMappingURL=astToSqlWhere.js.map