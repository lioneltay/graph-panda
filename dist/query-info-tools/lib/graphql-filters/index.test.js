var R = require("ramda");
var sqlFormatter = require("sql-formatter");
var generateWhereClause = require("./index").generateWhereClause;
var schema = {
    _typeMap: {
        User: {
            _typeConfig: {
                sqlTable: "\"user\"",
                uniqueKey: "id",
            },
            _fields: {
                createdAt: { sqlColumn: "createdat" },
                cost: { sqlColumn: "cost" },
                name: { sqlColumn: "name" },
                id: { sqlColumn: "id" },
                joinDate: { sqlColumn: "createdat" },
                somecount: { sqlExpr: "(SELECT COUNT(*) from \"the-table\")" },
                whatcount: { sqlExpr: function (table) { return "(SELECT COUNT(*) from " + table + ")"; } },
                nutritionLogs: {
                    sqlBatch: {
                        returnType: "NutritionLog",
                        thisKey: "userid",
                        parentKey: "id",
                    },
                },
            },
        },
        NutritionLog: {
            _typeConfig: {
                sqlTable: "\"user\"",
                uniqueKey: "id",
            },
            _fields: {
                amount: { sqlColumn: "amount" },
            },
        },
    },
};
test("generateWhereClause: No filters returns undefined", function () {
    expect(generateWhereClause()).toEqual(undefined);
});
test("generateWhereClause: No filters returns undefined (2)", function () {
    expect(generateWhereClause(undefined)).toEqual(undefined);
});
test("generateWhereClause: No filters returns undefined (3)", function () {
    expect(generateWhereClause(undefined, {})).toEqual(undefined);
});
test("generateWhereClause: No filters returns undefined (4)", function () {
    expect(generateWhereClause({})).toEqual(undefined);
});
test("generateWhereClause: No filters returns undefined (5)", function () {
    expect(generateWhereClause({}, {})).toEqual(undefined);
});
test("generateWhereClause: No filters returns undefined (6)", function () {
    expect(generateWhereClause({}, {
        type: "Payment",
        tableName: "table",
    })).toEqual(undefined);
});
test("generateWhereClause: test", function () {
    var filters = {
        createdAt: { gt: "2028-2-2" },
        OR: [
            { cost: { lt: 3 } },
            {
                AND: [{ id: { in: [2, 5, 8, 9] } }, { cost: { gt: 20 } }],
            },
        ],
    };
    var sql = sqlFormatter.format("\n  (\n    table.createdat > '2028-2-2'\n    AND (\n      table.cost < 3\n      OR (\n        table.id in (2,5,8,9)\n        AND table.cost > 20\n      )\n    )\n  )\n");
    expect(generateWhereClause(filters, { schema: schema, tableName: "table", type: "User" })).toEqual(sql);
});
test("generateWhereClause: test 2 complex", function () {
    var filters = {
        AND: [
            { createdAt: { gt: "2028-2-2" } },
            {
                OR: [
                    { cost: { lt: 3 } },
                    {
                        AND: [
                            { id: { in: [2, 5, 8, 9] } },
                            { cost: { gt: 20 } },
                            {
                                AND: [{ cost: { gt: 20 } }, { cost: { lt: 30 } }],
                            },
                        ],
                    },
                ],
            },
        ],
    };
    var sql = sqlFormatter.format("\n    (\n      table.createdat > '2028-2-2'\n      AND (\n        table.cost < 3\n        OR (\n          table.id in (2,5,8,9)\n          AND table.cost > 20\n          AND table.cost > 20\n          AND table.cost < 30\n        )\n      )\n    )\n  ");
    expect(generateWhereClause(filters, { schema: schema, tableName: "table", type: "User" })).toEqual(sql);
});
test("generateWhereClause: test 3 complex", function () {
    var filters = {
        AND: [
            { createdAt: { gt: "2028-2-2" } },
            {
                OR: [
                    { cost: { lt: 3 } },
                    {
                        AND: [
                            { id: { in: [2, 5, 8, 9] } },
                            { cost: { gt: 20 } },
                            {
                                OR: [{ name: { like: "A%" } }, { name: { regex: ".*A^" } }],
                            },
                            {
                                AND: [{ cost: { gt: 20 } }, { cost: { lt: 30 } }],
                            },
                        ],
                    },
                ],
            },
        ],
    };
    var sql = sqlFormatter.format("\n  (\n    table.createdat > '2028-2-2'\n    AND (\n      table.cost < 3\n      OR (\n        table.id in (2,5,8,9)\n        AND table.cost > 20\n        AND (\n          table.name LIKE 'A%'\n          OR table.name ~ '.*A^'\n        )\n        AND table.cost > 20\n        AND table.cost < 30\n      )\n    )\n  )\n");
    expect(generateWhereClause(filters, { schema: schema, tableName: "table", type: "User" })).toEqual(sql);
});
test("generateWhereClause: schemas", function () {
    var filters = {
        joinDate: { gt: "2028-2-2" },
        somecount: { gt: 1000 },
        OR: [
            { whatcount: { gt: 2000 } },
            { cost: { lt: 3 } },
            {
                AND: [{ id: { in: [2, 5, 8, 9] } }, { cost: { gt: 20 } }],
            },
        ],
    };
    var sql = sqlFormatter.format("\n    (\n      table.createdat > '2028-2-2'\n      AND (SELECT COUNT(*) from \"the-table\") > 1000\n      AND (\n        (SELECT COUNT(*) from table) > 2000\n        OR table.cost < 3\n        OR (\n          table.id in (2,5,8,9)\n          AND table.cost > 20\n        )\n      )\n    )\n  ");
    expect(generateWhereClause(filters, { schema: schema, tableName: "table", type: "User" })).toEqual(sql);
});
//# sourceMappingURL=index.test.js.map