var R = require("ramda");
var astToSqlWhere = require("./astToSqlWhere").astToSqlWhere;
var formattedFiltersToWhereAST = require("./formattedFiltersToWhereAST").formattedFiltersToWhereAST;
var schema = {
    _typeMap: {
        User: {
            _typeConfig: {
                sqlTable: "\"user\"",
                uniqueKey: "id",
            },
            _fields: {
                postCount: { sqlColumn: "postcount" },
                firstName: { sqlColumn: "firstname" },
                clubs: {
                    junction: {
                        returnType: "Club",
                        sqlTable: "\"member\"",
                        uniqueKey: "id",
                        sqlBatch: {
                            parentKey: "id",
                            thisKey: "userid",
                            sqlJoin: function (junction, payment) {
                                return junction + ".clubid = " + payment + ".id";
                            },
                        },
                    },
                },
                posts: {
                    sqlBatch: {
                        returnType: "Post",
                        thisKey: "userid",
                        parentKey: "id",
                    },
                },
            },
        },
        Post: {
            _typeConfig: {
                sqlTable: "\"post\"",
                uniqueKey: "id",
            },
            _fields: {
                content: { sqlColumn: "content" },
                author: {
                    sqlBatch: {
                        returnType: "User",
                        thisKey: "id",
                        parentKey: "userid",
                    },
                },
            },
        },
        Club: {
            _typeConfig: {
                sqlTable: "\"club\"",
                uniqueKey: "id",
            },
            _fields: {
                name: { sqlColumn: "namecolumn" },
                stars: { sqlColumn: "stars" },
            },
        },
        Payment: {
            _typeConfig: {
                sqlTable: "\"core-payment\"",
                uniqueKey: "id",
            },
            _fields: {
                amount: { sqlColumn: "amountfield" },
            },
        },
        Item: {
            _typeConfig: {
                sqlTable: "item-table",
                uniqueKey: "id",
            },
            _fields: {
                id: { sqlColumn: "id" },
                createdAt: { sqlColumn: "createdat" },
                joinDate: { sqlColumn: "joindate" },
                cost: { sqlColumn: "cost" },
                z: { sqlColumn: "zz" },
                y: { sqlColumn: "yy" },
                x: { sqlColumn: "xx" },
                donationsCount: { sqlExpr: "(SELECT COUNT(*) from \"the-table\")" },
                tableCount: { sqlExpr: function (table) { return "(SELECT COUNT(*) from " + table + ")"; } },
                surveyResponses: {
                    sqlBatch: { parentKey: "id", thisKey: "oxiluserid" },
                },
                donations: {
                    junction: {
                        parentTable: '"item-table"',
                        thisTable: '"core-payment"',
                        sqlTable: '"core-donor"',
                        uniqueKey: "id",
                        sqlBatch: {
                            parentKey: "id",
                            thisKey: "oxiluserid",
                            sqlJoin: function (junction, payment) {
                                return junction + ".id = " + payment + ".donorid";
                            },
                        },
                    },
                },
            },
        },
    },
};
test("formattedFiltersToWhereAST: NOT", function () {
    var formattedFilters = {
        NOT: {
            AND: [
                { createdAt: { gt: "2028-2-2" } },
                { joinDate: { lt: 3 } },
                { joinDate: { gt: 7 } },
                { joinDate: { in: [1, 2, 3, 4, 5] } },
            ],
        },
    };
    var whereAST = {
        NOT: {
            AND: [
                { clause: "table.createdat > ?", variables: ["2028-2-2"] },
                { clause: "table.joindate < ?", variables: [3] },
                { clause: "table.joindate > ?", variables: [7] },
                {
                    clause: "table.joindate in (?, ?, ?, ?, ?)",
                    variables: [1, 2, 3, 4, 5],
                },
            ],
        },
    };
    expect(formattedFiltersToWhereAST(formattedFilters, {
        schema: schema,
        type: "Item",
        tableName: "table",
    })).toEqual(whereAST);
});
test("formattedFiltersToWhereAST: test 1", function () {
    var formattedFilters = {
        AND: [
            { createdAt: { gt: "2028-2-2" } },
            { joinDate: { lt: 3 } },
            { joinDate: { gt: 7 } },
            { joinDate: { in: [1, 2, 3, 4, 5] } },
        ],
    };
    var whereAST = {
        AND: [
            { clause: "table.createdat > ?", variables: ["2028-2-2"] },
            { clause: "table.joindate < ?", variables: [3] },
            { clause: "table.joindate > ?", variables: [7] },
            {
                clause: "table.joindate in (?, ?, ?, ?, ?)",
                variables: [1, 2, 3, 4, 5],
            },
        ],
    };
    expect(formattedFiltersToWhereAST(formattedFilters, {
        schema: schema,
        type: "Item",
        tableName: "table",
    })).toEqual(whereAST);
});
test("formattedFiltersToWhereAST: more complex", function () {
    var formattedFilters = {
        AND: [
            { createdAt: { gt: "2028-2-2" } },
            {
                OR: [
                    {
                        AND: [{ cost: { gt: 3 } }, { cost: { lt: 5 } }],
                    },
                    {
                        AND: [{ id: { in: [2, 5, 8, 9] } }, { cost: { gt: 20 } }],
                    },
                ],
            },
        ],
    };
    var ast = {
        AND: [
            { clause: "table.createdat > ?", variables: ["2028-2-2"] },
            {
                OR: [
                    {
                        AND: [
                            { clause: "table.cost > ?", variables: [3] },
                            { clause: "table.cost < ?", variables: [5] },
                        ],
                    },
                    {
                        AND: [
                            { clause: "table.id in (?, ?, ?, ?)", variables: [2, 5, 8, 9] },
                            { clause: "table.cost > ?", variables: [20] },
                        ],
                    },
                ],
            },
        ],
    };
    expect(formattedFiltersToWhereAST(formattedFilters, {
        tableName: "table",
        type: "Item",
        schema: schema,
    })).toEqual(ast);
});
test("formattedFiltesrToWhereAST: handles sqlColumn aliases", function () {
    var formattedFilters = {
        AND: [
            { joinDate: { gt: "2028-2-2" } },
            { donationsCount: { gt: 1000 } },
            { tableCount: { gt: 2000 } },
            {
                OR: [
                    {
                        AND: [{ cost: { gt: 3 } }, { cost: { lt: 5 } }],
                    },
                    {
                        AND: [{ id: { in: [2, 5, 8, 9] } }, { cost: { gt: 20 } }],
                    },
                ],
            },
        ],
    };
    var ast = {
        AND: [
            { clause: "\"custom-table\".joindate > ?", variables: ["2028-2-2"] },
            { clause: "(SELECT COUNT(*) from \"the-table\") > ?", variables: [1000] },
            {
                clause: "(SELECT COUNT(*) from \"custom-table\") > ?",
                variables: [2000],
            },
            {
                OR: [
                    {
                        AND: [
                            { clause: "\"custom-table\".cost > ?", variables: [3] },
                            { clause: "\"custom-table\".cost < ?", variables: [5] },
                        ],
                    },
                    {
                        AND: [
                            {
                                clause: "\"custom-table\".id in (?, ?, ?, ?)",
                                variables: [2, 5, 8, 9],
                            },
                            { clause: "\"custom-table\".cost > ?", variables: [20] },
                        ],
                    },
                ],
            },
        ],
    };
    expect(formattedFiltersToWhereAST(formattedFilters, {
        schema: schema,
        tableName: "\"custom-table\"",
        type: "Item",
    })).toEqual(ast);
});
test("formattedFiltesrToWhereAST: Handles junction batch SOME", function () {
    var formattedFilters = {
        clubs: {
            SOME: {
                OR: [{ name: { gt: 10 } }, { stars: { gt: 4 } }],
            },
        },
    };
    var ast = {
        joins: function (where) { return "\"user-alias\".id IN (\n      SELECT \"user\".id FROM \"user\"\n        INNER JOIN \"member\" ON \"member\".userid = \"user\".id\n        INNER JOIN \"club\" ON \"member\".clubid = \"club\".id\n      WHERE " + where + "\n    )"; },
        subClause: {
            OR: [
                { clause: "\"club\".namecolumn > ?", variables: [10] },
                { clause: "\"club\".stars > ?", variables: [4] },
            ],
        },
    };
    var res = formattedFiltersToWhereAST(formattedFilters, {
        schema: schema,
        tableName: "\"user-alias\"",
        type: "User",
    });
    expect(astToSqlWhere(res)).toEqual(astToSqlWhere(ast));
});
test("formattedFiltesrToWhereAST: Handles junction batch SOME (2)", function () {
    var formattedFilters = {
        clubs: {
            SOME: {
                OR: [{ name: { gt: 10 } }, { stars: { gt: 4 } }],
            },
        },
    };
    var ast = {
        joins: function (where) { return "\"user-alias\".id IN (\n      SELECT \"user\".id FROM \"user\"\n        INNER JOIN \"member\" ON \"member\".userid = \"user\".id\n        INNER JOIN \"club\" ON \"member\".clubid = \"club\".id\n      WHERE " + where + "\n    )"; },
        subClause: {
            OR: [
                { clause: "\"club\".namecolumn > ?", variables: [10] },
                { clause: "\"club\".stars > ?", variables: [4] },
            ],
        },
    };
    var res = formattedFiltersToWhereAST(formattedFilters, {
        schema: schema,
        tableName: "\"user-alias\"",
        type: "User",
    });
    expect(astToSqlWhere(res)).toEqual(astToSqlWhere(ast));
});
test("formattedFiltesrToWhereAST: Handles junction batch SOME (3)", function () {
    var formattedFilters = {
        OR: [
            {
                clubs: {
                    SOME: {
                        OR: [{ name: { gt: 10 } }, { stars: { gt: 4 } }],
                    },
                },
            },
            {
                clubs: {
                    SOME: {
                        OR: [{ name: { gt: 1 } }, { stars: { gt: 40 } }],
                    },
                },
            },
        ],
    };
    var ast = {
        OR: [
            {
                joins: function (where) { return "\"user-alias\".id IN (\n          SELECT \"user\".id FROM \"user\"\n            INNER JOIN \"member\" ON \"member\".userid = \"user\".id\n            INNER JOIN \"club\" ON \"member\".clubid = \"club\".id\n          WHERE " + where + "\n        )"; },
                subClause: {
                    OR: [
                        { clause: "\"club\".namecolumn > ?", variables: [10] },
                        { clause: "\"club\".stars > ?", variables: [4] },
                    ],
                },
            },
            {
                joins: function (where) { return "\"user-alias\".id IN (\n          SELECT \"user\".id FROM \"user\"\n            INNER JOIN \"member\" ON \"member\".userid = \"user\".id\n            INNER JOIN \"club\" ON \"member\".clubid = \"club\".id\n          WHERE " + where + "\n        )"; },
                subClause: {
                    OR: [
                        { clause: "\"club\".namecolumn > ?", variables: [1] },
                        { clause: "\"club\".stars > ?", variables: [40] },
                    ],
                },
            },
        ],
    };
    var res = formattedFiltersToWhereAST(formattedFilters, {
        schema: schema,
        tableName: "\"user-alias\"",
        type: "User",
    });
    expect(astToSqlWhere(res)).toEqual(astToSqlWhere(ast));
});
test("formattedFiltesrToWhereAST: Handles junction batch NONE", function () {
    var formattedFilters = {
        clubs: {
            NONE: {
                OR: [{ name: { gt: 10 } }, { stars: { gt: 4 } }],
            },
        },
    };
    var ast = {
        joins: function (where) { return "\"user-alias\".id NOT IN (\n      SELECT \"user\".id FROM \"user\"\n        INNER JOIN \"member\" ON \"member\".userid = \"user\".id\n        INNER JOIN \"club\" ON \"member\".clubid = \"club\".id\n      WHERE " + where + "\n    )"; },
        subClause: {
            OR: [
                { clause: "\"club\".namecolumn > ?", variables: [10] },
                { clause: "\"club\".stars > ?", variables: [4] },
            ],
        },
    };
    var res = formattedFiltersToWhereAST(formattedFilters, {
        schema: schema,
        tableName: "\"user-alias\"",
        type: "User",
    });
    expect(astToSqlWhere(res)).toEqual(astToSqlWhere(ast));
});
test("formattedFiltesrToWhereAST: Handles junction batch ALL", function () {
    var formattedFilters = {
        clubs: {
            ALL: {
                OR: [{ name: { gt: 10 } }, { stars: { gt: 4 } }],
            },
        },
    };
    var ast = {
        joins: function (where) { return "\"user-alias\".id NOT IN (\n      SELECT \"user\".id FROM \"user\"\n        INNER JOIN \"member\" ON \"member\".userid = \"user\".id\n        INNER JOIN \"club\" ON \"member\".clubid = \"club\".id\n      WHERE NOT " + where + "\n    )"; },
        subClause: {
            OR: [
                { clause: "\"club\".namecolumn > ?", variables: [10] },
                { clause: "\"club\".stars > ?", variables: [4] },
            ],
        },
    };
    var res = formattedFiltersToWhereAST(formattedFilters, {
        schema: schema,
        tableName: "\"user-alias\"",
        type: "User",
    });
    expect(astToSqlWhere(res)).toEqual(astToSqlWhere(ast));
});
test("formattedFiltesrToWhereAST: Handles sqlBatch (many-to-one) ", function () {
    var formattedFilters = {
        author: {
            postCount: { gt: 3 },
        },
    };
    var ast = {
        joins: function (where) { return "\"post-alias\".id IN (\n      SELECT \"post\".id FROM \"post\"\n        INNER JOIN \"user\" ON \"user\".id = \"post\".userid\n      WHERE " + where + "\n    )"; },
        subClause: {
            clause: "\"user\".postcount > ?",
            variables: [3],
        },
    };
    var res = formattedFiltersToWhereAST(formattedFilters, {
        schema: schema,
        tableName: "\"post-alias\"",
        type: "Post",
    });
    expect(astToSqlWhere(res)).toEqual(astToSqlWhere(ast));
});
test("formattedFiltesrToWhereAST: Handles sqlBatch (many-to-one) (2)", function () {
    var formattedFilters = {
        author: {
            OR: [{ postCount: { gt: 3 } }, { firstName: { eq: "bob" } }],
        },
    };
    var ast = {
        joins: function (where) { return "\"post-alias\".id IN (\n      SELECT \"post\".id FROM \"post\"\n        INNER JOIN \"user\" ON \"user\".id = \"post\".userid\n      WHERE " + where + "\n    )"; },
        subClause: {
            OR: [
                { clause: "\"user\".postcount > ?", variables: [3] },
                { clause: "\"user\".firstname = ?", variables: ["bob"] },
            ],
        },
    };
    var res = formattedFiltersToWhereAST(formattedFilters, {
        schema: schema,
        tableName: "\"post-alias\"",
        type: "Post",
    });
    expect(astToSqlWhere(res)).toEqual(astToSqlWhere(ast));
});
test("formattedFiltesrToWhereAST: Handles sqlBatch (many-to-one) (3)", function () {
    var formattedFilters = {
        posts: {
            SOME: {
                content: { eq: "foo" },
            },
        },
    };
    var ast = {
        joins: function (where) { return "\"user-alias\".id IN (\n      SELECT \"user\".id FROM \"user\"\n        INNER JOIN \"post\" ON \"post\".userid = \"user\".id\n      WHERE " + where + "\n    )"; },
        subClause: {
            clause: "\"post\".content = ?",
            variables: ["foo"],
        },
    };
    var res = formattedFiltersToWhereAST(formattedFilters, {
        schema: schema,
        tableName: "\"user-alias\"",
        type: "User",
    });
    expect(astToSqlWhere(res)).toEqual(astToSqlWhere(ast));
});
//# sourceMappingURL=formattedFiltersToWhereAST.test.js.map