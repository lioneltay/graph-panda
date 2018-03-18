var R = require("ramda");
// const fieldReturnType = ({ schema, type, field }) => {
//   const { GraphQLScalarType, GraphQLList } = require("graphql")
//   const typeObject = readTypeField({ schema, type, field }).type
//   if (typeObject instanceof GraphQLScalarType) {
//     return typeObject.name
//   }
//   if (typeObject instanceof GraphQLList) {
//     return typeObject.ofType.name
//   }
// }
var readType = function (_a) {
    var schema = _a.schema, type = _a.type;
    return schema._typeMap[type];
};
var readField = function (_a) {
    var schema = _a.schema, type = _a.type, field = _a.field;
    var resField = schema._typeMap[type]._fields[field];
    if (R.isNil(resField)) {
        throw Error("Field: [" + field + "] does not exist on the schema");
    }
    return resField;
};
var readJunction = function (_a) {
    var schema = _a.schema, type = _a.type, field = _a.field;
    try {
        return schema._typeMap[type]._fields[field].junction;
    }
    catch (e) {
        throw Error("readJunction(" + type + ", " + field + ")");
    }
};
var readSqlBatch = function (_a) {
    var schema = _a.schema, type = _a.type, field = _a.field;
    return readField({ schema: schema, type: type, field: field }).sqlBatch;
};
var readSqlColumn = function (_a) {
    var schema = _a.schema, type = _a.type, field = _a.field;
    return schema._typeMap[type]._fields[field].sqlColumn;
};
var readSqlExpr = function (_a) {
    var schema = _a.schema, type = _a.type, field = _a.field;
    return schema._typeMap[type]._fields[field].sqlExpr;
};
var readSqlTable = function (_a) {
    var schema = _a.schema, type = _a.type;
    return schema._typeMap[type]._typeConfig.sqlTable;
};
var readUniqueKey = function (_a) {
    var schema = _a.schema, type = _a.type;
    return schema._typeMap[type]._typeConfig.uniqueKey;
};
var readReturnType = function (_a) {
    var schema = _a.schema, type = _a.type, field = _a.field;
    var returnType;
    var sqlBatch = readSqlBatch({ schema: schema, type: type, field: field });
    if (sqlBatch) {
        returnType = sqlBatch.returnType;
    }
    var junction = readJunction({ schema: schema, type: type, field: field });
    if (junction) {
        returnType = junction.returnType;
    }
    if (R.isNil(returnType)) {
        throw Error("Field [" + field + "] of Type [" + type + "] does not have returnType specified");
    }
    if (R.isNil(readType({ type: returnType, schema: schema }))) {
        throw Error("returnType: [" + returnType + "] does not exist on the schema");
    }
    return returnType;
};
var traverseSchema = function (_a) {
    var schema = _a.schema;
    // do stuff
};
var traverseSchemaWithContext = function (_a) {
    var schema = _a.schema;
    // do stuff
};
var wrapResolvers = function (_a) {
    var schema = _a.schema;
    // do stuff
};
module.exports = {
    readSqlColumn: readSqlColumn,
    readSqlExpr: readSqlExpr,
    readSqlTable: readSqlTable,
    readUniqueKey: readUniqueKey,
    readSqlBatch: readSqlBatch,
    readJunction: readJunction,
    readReturnType: readReturnType,
};
//# sourceMappingURL=index.js.map