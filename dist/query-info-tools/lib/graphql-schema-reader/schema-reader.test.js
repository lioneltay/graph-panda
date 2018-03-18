var _a = require("./index"), readSqlColumn = _a.readSqlColumn, readSqlExpr = _a.readSqlExpr, readSqlTable = _a.readSqlTable, readUniqueKey = _a.readUniqueKey, readReturnType = _a.readReturnType;
var schema = {
    _typeMap: {
        Item: {
            _typeConfig: {
                sqlTable: "item-table",
                uniqueKey: "id",
            },
            _fields: {
                fieldHere: {
                    sqlColumn: "test-column",
                    sqlExpr: "test-expr",
                },
                batchField: {
                    sqlBatch: {
                        returnType: "BatchAnswer",
                        thisKey: "foo",
                        parentKey: "bar",
                    },
                },
                junctionField: {
                    junction: {
                        returnType: "JunctionAnswer",
                        uniqueKey: "id",
                        sqlTable: "junction-table",
                        sqlBatch: {
                            thisKey: "id",
                            parentKey: "id",
                            sqlJoin: function (x, y) { return ""; },
                        },
                    },
                },
            },
        },
        JunctionAnswer: {},
        BatchAnswer: {},
    },
};
test("schema-reader: sqlColumn", function () {
    expect(readSqlColumn({ type: "Item", field: "fieldHere", schema: schema })).toEqual("test-column");
});
test("schema-reader: sqlExpr", function () {
    expect(readSqlExpr({ type: "Item", field: "fieldHere", schema: schema })).toEqual("test-expr");
});
test("schema-reader: sqlTable", function () {
    expect(readSqlTable({ type: "Item", schema: schema })).toEqual("item-table");
});
test("schema-reader: uniqueKey", function () {
    expect(readUniqueKey({ type: "Item", schema: schema })).toEqual("id");
});
test("schema-reader: readReturnType batch", function () {
    expect(readReturnType({ type: "Item", schema: schema, field: "batchField" })).toEqual("BatchAnswer");
});
test("schema-reader: readReturnType junction", function () {
    expect(readReturnType({ type: "Item", schema: schema, field: "junctionField" })).toEqual("JunctionAnswer");
});
//# sourceMappingURL=schema-reader.test.js.map