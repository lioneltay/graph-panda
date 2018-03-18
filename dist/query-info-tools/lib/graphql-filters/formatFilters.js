var R = require("ramda");
var _a = require("./helpers"), objToArray = _a.objToArray, onlyKey = _a.onlyKey, onlyValue = _a.onlyValue, singleKey = _a.singleKey;
var _b = require("../graphql-schema-reader"), readSqlBatch = _b.readSqlBatch, readJunction = _b.readJunction, readReturnType = _b.readReturnType;
/**
 * Need to detect whether a field is a relation field and recurse
 * Need to detect relational key and recurse
 */
var sqlFilterKey = function (key) {
    return R.contains(key, [
        "gt",
        "lt",
        "in",
        "regex",
        "like",
        "eq",
        "gte",
        "lte",
        "null",
    ]);
};
var relationalFilterKey = function (key) { return R.contains(key, ["some", "all", "none"]); };
var combinationKey = function (key) { return R.contains(key, ["AND", "OR"]); };
var relationalField = function (_a) {
    var schema = _a.schema, type = _a.type, field = _a.field;
    return (readSqlBatch({ schema: schema, type: type, field: field }) ||
        readJunction({ schema: schema, type: type, field: field }));
};
var formatOR = function (orFilters, _a) {
    var schema = _a.schema, type = _a.type;
    var res = orFilters.OR.map(function (filter) {
        return formatFilters(filter, { schema: schema, type: type });
    }).reduce(function (acc, v) {
        if (R.has("AND", v) && singleKey(v.AND)) {
            return acc.concat(v.AND);
        }
        if (R.has("OR", v)) {
            return acc.concat(v.OR);
        }
        return acc.concat(v);
    }, []);
    return { OR: res };
};
var formatAND = function (andFilters, _a) {
    var schema = _a.schema, type = _a.type;
    var arr = andFilters.AND.map(function (filter) {
        return formatFilters(filter, { schema: schema, type: type });
    }).reduce(function (acc, v) {
        if (R.has("AND", v)) {
            return acc.concat(v.AND);
        }
        if (R.has("OR", v) && singleKey(v.OR)) {
            return acc.concat(v.OR);
        }
        return acc.concat(v);
    }, []);
    return { AND: arr };
};
var formatFilters = function (filters, _a) {
    var schema = _a.schema, type = _a.type;
    // { property: { ... }, proerty2: { ... }, ... }
    if (!singleKey(filters)) {
        var res = formatFilters({
            AND: objToArray(filters),
        }, { schema: schema, type: type });
        return res;
    }
    // { AND/or: [{}]}
    if (singleKey(filters) &&
        combinationKey(onlyKey(filters)) &&
        onlyValue(filters).length === 1) {
        return formatFilters(onlyValue(filters)[0], { schema: schema, type: type });
    }
    // { AND: [...] }
    if (onlyKey(filters) === "AND") {
        return formatAND(filters, { schema: schema, type: type });
    }
    // { OR: [...] }
    if (onlyKey(filters) === "OR") {
        return formatOR(filters, { schema: schema, type: type });
    }
    // { value: { NOT: { gt: 3 } } }
    if (singleKey(onlyValue(filters)) && onlyKey(onlyValue(filters)) === "NOT") {
        return {
            NOT: formatFilters((_b = {}, _b[onlyKey(filters)] = onlyValue(onlyValue(filters)), _b), { schema: schema, type: type }),
        };
    }
    // { NOT: { value: { gt: 3 } } }
    if (singleKey(onlyValue(filters)) && onlyKey(filters) === "NOT") {
        return {
            NOT: formatFilters(onlyValue(filters), { schema: schema, type: type }),
        };
    }
    // Relational field (has sqlBatch or junction in schema)
    if (relationalField({ schema: schema, type: type, field: onlyKey(filters) })) {
        var field_1 = onlyKey(filters); // user/post/bla
        var filterProps = onlyValue(filters); // object of filters
        if (!singleKey(filterProps)) {
            return formatFilters({
                AND: objToArray(filterProps).map(function (val) {
                    return (_a = {}, _a[field_1] = val, _a);
                    var _a;
                }),
            }, { schema: schema, type: type });
        }
        if (combinationKey(onlyKey(filterProps))) {
            return _c = {},
                _c[onlyKey(filterProps)] = onlyValue(filterProps).map(function (val) {
                    return formatFilters((_a = {}, _a[field_1] = val, _a), { schema: schema, type: type });
                    var _a;
                }),
                _c;
        }
        if (["SOME", "ALL", "NONE"].includes(onlyKey(filterProps))) {
            return _d = {},
                _d[field_1] = (_e = {},
                    _e[onlyKey(filterProps)] = formatFilters(onlyValue(filterProps), {
                        schema: schema,
                        type: readReturnType({ schema: schema, type: type, field: field_1 }),
                    }),
                    _e),
                _d;
        }
        else {
            return _f = {},
                _f[field_1] = formatFilters(filterProps, {
                    schema: schema,
                    type: readReturnType({ schema: schema, type: type, field: field_1 }),
                }),
                _f;
        }
    }
    // { property: { filterType: value } }
    if (singleKey(onlyValue(filters)) &&
        sqlFilterKey(onlyKey(onlyValue(filters)))) {
        return filters;
    }
    // // { property: { relationalType: value } }
    // if (
    //   singleKey(onlyValue(filters)) &&
    //   relationalFilterKey(onlyKey(onlyValue(filters)))
    // ) {
    //   return filters
    // }
    // { property; { ... } }
    if (!combinationKey(onlyKey(filters)) && !singleKey(onlyValue(filters))) {
        return formatFilters({
            AND: objToArray(onlyValue(filters)).map(function (clause) {
                return (_a = {},
                    _a[onlyKey(filters)] = clause,
                    _a);
                var _a;
            }),
        }, { schema: schema, type: type });
    }
    var _b, _c, _d, _e, _f;
};
module.exports = {
    formatFilters: formatFilters,
};
//# sourceMappingURL=formatFilters.js.map