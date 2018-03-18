var R = require("ramda");
var _a = require("./helpers"), onlyKey = _a.onlyKey, onlyValue = _a.onlyValue;
var _b = require("../graphql-schema-reader"), readSqlColumn = _b.readSqlColumn, readSqlExpr = _b.readSqlExpr, readJunction = _b.readJunction, readSqlBatch = _b.readSqlBatch, readSqlTable = _b.readSqlTable, readUniqueKey = _b.readUniqueKey;
var traverseFormattedFilters = function (filters, fn) {
    var key = onlyKey(filters);
    var value = onlyValue(filters);
    if (R.contains(key, ["AND", "OR"])) {
        return _a = {}, _a[key] = value.map(function (filter) { return traverseFormattedFilters(filter, fn); }), _a;
    }
    return fn(filters);
    var _a;
};
var mapFieldName = function (_a) {
    var type = _a.type, tableName = _a.tableName, _b = _a.schema, schema = _b === void 0 ? {} : _b, fieldName = _a.fieldName;
    var col = readSqlColumn({ schema: schema, type: type, field: fieldName });
    var expr = readSqlExpr({ schema: schema, type: type, field: fieldName });
    return col
        ? tableName + "." + col
        : expr
            ? "" + (R.type(expr) === "Function" ? expr(tableName) : expr)
            : tableName + "." + fieldName;
};
var parseJunctionField = function (clause, _a) {
    var type = _a.type, tableName = _a.tableName, schema = _a.schema;
    var fieldName = onlyKey(clause);
    var filters = onlyValue(clause);
    var key = onlyKey(filters);
    var value = onlyValue(filters);
    var junction = readJunction({ schema: schema, type: type, field: fieldName });
    var junctionTable = junction.sqlTable, sqlBatch = junction.sqlBatch, returnType = junction.returnType;
    var parentKey = sqlBatch.parentKey, thisKey = sqlBatch.thisKey, sqlJoin = sqlBatch.sqlJoin;
    var parentUniqueKey = readUniqueKey({ schema: schema, type: type });
    var parentTable = readSqlTable({ schema: schema, type: type });
    var thisTable = readSqlTable({ schema: schema, type: returnType });
    var generateJoins = function (_a) {
        var inSet = _a.inSet, negateWhere = _a.negateWhere;
        return function (where) { return "\n      " + tableName + "." + parentUniqueKey + " " + (inSet ? "IN" : "NOT IN") + " (\n        SELECT " + parentTable + "." + parentKey + " FROM " + parentTable + "\n          INNER JOIN " + junctionTable + " ON " + junctionTable + "." + thisKey + " = " + parentTable + "." + parentKey + "\n          INNER JOIN " + thisTable + " ON " + sqlJoin(junctionTable, thisTable) + "\n        WHERE " + (negateWhere ? "NOT " : "") + where + "\n      )\n    "; };
    };
    var subClause = formattedFiltersToWhereAST(value, {
        type: returnType,
        schema: schema,
        tableName: readSqlTable({ schema: schema, type: returnType }),
    });
    if (key === "SOME") {
        return {
            joins: generateJoins({ inSet: true, negateWhere: false }),
            subClause: subClause,
        };
    }
    if (key === "NONE") {
        return {
            joins: generateJoins({ inSet: false, negateWhere: false }),
            subClause: subClause,
        };
    }
    if (key === "ALL") {
        return {
            joins: generateJoins({ inSet: false, negateWhere: true }),
            subClause: subClause,
        };
    }
};
var parseSqlBatchField = function (clause, _a) {
    var type = _a.type, tableName = _a.tableName, schema = _a.schema;
    var fieldName = onlyKey(clause);
    var filters = onlyValue(clause);
    var key = onlyKey(filters);
    var value = onlyValue(filters);
    var sqlBatch = readSqlBatch({ schema: schema, type: type, field: fieldName });
    // Relational Fields
    var returnType = sqlBatch.returnType, thisKey = sqlBatch.thisKey, parentKey = sqlBatch.parentKey;
    var parentUniqueKey = readUniqueKey({ schema: schema, type: type });
    var parentTable = readSqlTable({ schema: schema, type: type });
    var thisTable = readSqlTable({ schema: schema, type: returnType });
    var generateJoins = function (_a) {
        var inSet = _a.inSet, negateWhere = _a.negateWhere;
        return function (where) { return "\n    " + tableName + "." + parentUniqueKey + " " + (inSet ? "IN" : "NOT IN") + " (\n      SELECT " + parentTable + "." + parentUniqueKey + " FROM " + parentTable + "\n        INNER JOIN " + thisTable + " ON " + thisTable + "." + thisKey + " = " + parentTable + "." + parentKey + "\n      WHERE " + (negateWhere ? "NOT " : "") + where + "\n    )\n  "; };
    };
    var generateSubClause = function (filters) {
        return formattedFiltersToWhereAST(filters, {
            type: returnType,
            schema: schema,
            tableName: readSqlTable({ schema: schema, type: returnType }),
        });
    };
    if (key === "SOME") {
        return {
            joins: generateJoins({ inSet: true, negateWhere: false }),
            subClause: generateSubClause(value),
        };
    }
    if (key === "NONE") {
        return {
            joins: generateJoins({ inSet: false, negateWhere: false }),
            subClause: generateSubClause(value),
        };
    }
    if (key === "ALL") {
        return {
            joins: generateJoins({ inSet: false, negateWhere: true }),
            subClause: generateSubClause(value),
        };
    }
    return {
        joins: generateJoins({ inSet: true, negateWhere: false }),
        subClause: generateSubClause(filters),
    };
};
var parsePropertyField = function (clause, _a) {
    var type = _a.type, tableName = _a.tableName, schema = _a.schema;
    var fieldName = onlyKey(clause);
    var filters = onlyValue(clause);
    var key = onlyKey(filters);
    var value = onlyValue(filters);
    var mappedField = mapFieldName({ type: type, fieldName: fieldName, schema: schema, tableName: tableName });
    if (key === "null") {
        return {
            clause: mappedField + " " + (value ? "IS NULL" : "IS NOT NULL"),
            variables: [],
        };
    }
    if (key === "eq") {
        return {
            clause: mappedField + " = ?",
            variables: [value],
        };
    }
    if (key === "gt") {
        return {
            clause: mappedField + " > ?",
            variables: [value],
        };
    }
    if (key === "gte") {
        return {
            clause: mappedField + " >= ?",
            variables: [value],
        };
    }
    if (key === "lt") {
        return {
            clause: mappedField + " < ?",
            variables: [value],
        };
    }
    if (key === "lte") {
        return {
            clause: mappedField + " <= ?",
            variables: [value],
        };
    }
    if (key === "regex") {
        return {
            clause: mappedField + " ~ ?",
            variables: [value],
        };
    }
    if (key === "like") {
        return {
            clause: mappedField + " LIKE ?",
            variables: [value],
        };
    }
    if (key === "in") {
        return {
            clause: mappedField + " in (" + value.map(function (_) { return "?"; }).join(", ") + ")",
            variables: value,
        };
    }
    throw Error("Invalid filter key [" + key + "]");
};
var parseClause = function (clause, _a) {
    var type = _a.type, tableName = _a.tableName, schema = _a.schema;
    var fieldName = onlyKey(clause);
    if (fieldName === "NOT") {
        return {
            NOT: formattedFiltersToWhereAST(onlyValue(clause), {
                type: type,
                tableName: tableName,
                schema: schema,
            }),
        };
    }
    // Relational Fields
    if (readJunction({ schema: schema, type: type, field: fieldName })) {
        return parseJunctionField(clause, { type: type, tableName: tableName, schema: schema });
    }
    if (readSqlBatch({ schema: schema, type: type, field: fieldName })) {
        return parseSqlBatchField(clause, { type: type, tableName: tableName, schema: schema });
    }
    // Property Fields
    return parsePropertyField(clause, { type: type, tableName: tableName, schema: schema });
};
var formattedFiltersToWhereAST = function (formattedFilters, _a) {
    var type = _a.type, tableName = _a.tableName, schema = _a.schema;
    return traverseFormattedFilters(formattedFilters, function (filter) {
        return parseClause(filter, { type: type, tableName: tableName, schema: schema });
    });
};
module.exports = {
    formattedFiltersToWhereAST: formattedFiltersToWhereAST,
};
//# sourceMappingURL=formattedFiltersToWhereAST.js.map