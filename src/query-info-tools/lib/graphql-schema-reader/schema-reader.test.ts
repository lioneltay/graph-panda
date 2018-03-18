const {
  readSqlColumn,
  readSqlExpr,
  readSqlTable,
  readUniqueKey,
  readReturnType,
} = require("./index")

const schema = {
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
              sqlJoin: (x, y) => ``,
            },
          },
        },
      },
    },

    JunctionAnswer: {},
    BatchAnswer: {},
  },
}

test("schema-reader: sqlColumn", () => {
  expect(readSqlColumn({ type: "Item", field: "fieldHere", schema })).toEqual(
    "test-column"
  )
})

test("schema-reader: sqlExpr", () => {
  expect(readSqlExpr({ type: "Item", field: "fieldHere", schema })).toEqual(
    "test-expr"
  )
})

test("schema-reader: sqlTable", () => {
  expect(readSqlTable({ type: "Item", schema })).toEqual("item-table")
})

test("schema-reader: uniqueKey", () => {
  expect(readUniqueKey({ type: "Item", schema })).toEqual("id")
})

test("schema-reader: readReturnType batch", () => {
  expect(readReturnType({ type: "Item", schema, field: "batchField" })).toEqual(
    "BatchAnswer"
  )
})

test("schema-reader: readReturnType junction", () => {
  expect(
    readReturnType({ type: "Item", schema, field: "junctionField" })
  ).toEqual("JunctionAnswer")
})
