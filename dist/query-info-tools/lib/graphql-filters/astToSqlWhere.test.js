var R = require("ramda");
var sqlFormatter = require("sql-formatter");
var formatSql = require("sql-formatter").format;
var astToSqlWhere = require("./astToSqlWhere").astToSqlWhere;
test("astToSqlWhere: Handles single clause", function () {
    var ast = {
        clause: "table.createdAt > ?",
        variables: ["2028-2-2"],
    };
    var sql = sqlFormatter.format("\n    table.createdAt > '2028-2-2'\n  ");
    expect(astToSqlWhere(ast)).toEqual(sql);
});
test("astToSqlWhere: test", function () {
    var ast = {
        AND: [
            { clause: "table.createdAt > ?", variables: ["2028-2-2"] },
            {
                OR: [
                    { clause: "table.cost < ?", variables: [3] },
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
    var sql = sqlFormatter.format("\n  (\n    table.createdAt > '2028-2-2'\n    AND (\n      table.cost < 3\n      OR (\n        table.id in (2,5,8,9)\n        AND table.cost > 20\n      )\n    )\n  )\n");
    expect(astToSqlWhere(ast)).toEqual(sql);
});
test("astToSqlWhere: test 2 complex", function () {
    var ast = {
        AND: [
            { clause: "table.createdAt > ?", variables: ["2028-2-2"] },
            {
                OR: [
                    { clause: "table.cost < ?", variables: [3] },
                    {
                        AND: [
                            { clause: "table.id in (?, ?, ?, ?)", variables: [2, 5, 8, 9] },
                            { clause: "table.cost > ?", variables: [20] },
                            {
                                AND: [
                                    { clause: "table.cost > ?", variables: [20] },
                                    { clause: "table.cost < ?", variables: [30] },
                                ],
                            },
                        ],
                    },
                ],
            },
        ],
    };
    var sql = sqlFormatter.format("\n  (\n    table.createdAt > '2028-2-2'\n    AND (\n      table.cost < 3\n      OR (\n        table.id in (2,5,8,9)\n        AND table.cost > 20\n        AND (\n          table.cost > 20\n          AND table.cost < 30\n        )\n      )\n    )\n  )\n");
    expect(astToSqlWhere(ast)).toEqual(sql);
});
test("astToSqlWhere: test 3 complex", function () {
    var ast = {
        AND: [
            { clause: "table.createdAt > ?", variables: ["2028-2-2"] },
            {
                OR: [
                    { clause: "table.cost < ?", variables: [3] },
                    {
                        AND: [
                            { clause: "table.id in (?, ?, ?, ?)", variables: [2, 5, 8, 9] },
                            { clause: "table.cost > ?", variables: [20] },
                            {
                                OR: [
                                    { clause: "table.name like ?", variables: ["A%"] },
                                    { clause: "table.name regex ?", variables: [".*A^"] },
                                ],
                            },
                            {
                                AND: [
                                    { clause: "table.cost > ?", variables: [20] },
                                    { clause: "table.cost < ?", variables: [30] },
                                ],
                            },
                        ],
                    },
                ],
            },
        ],
    };
    var sql = sqlFormatter.format("\n    (\n      table.createdAt > '2028-2-2'\n      AND (\n        table.cost < 3\n        OR (\n          table.id in (2,5,8,9)\n          AND table.cost > 20\n          AND (\n            table.name like 'A%'\n            OR table.name regex '.*A^'\n          )\n          AND (\n            table.cost > 20\n            AND table.cost < 30\n          )\n        )\n      )\n    )\n  ");
    expect(astToSqlWhere(ast)).toEqual(sql);
});
test("astToSqlWhere: Handles relational clauses (1)", function () {
    var filters = {
        posts: {
            some: {
                likes: { gt: 3 },
            },
        },
    };
    var ast = {
        joins: function (where) { return "user.inkey in (\n      SELECT user.inkey FROM user\n        INNER JOIN post ON post.authorid = user.id\n      WHERE " + where + "\n    )"; },
        subClause: { clause: "post.likes > ?", variables: [3] },
    };
    var sql = sqlFormatter.format("\n    user.inkey in (\n      SELECT user.inkey FROM user\n        INNER JOIN post ON post.authorid = user.id\n      WHERE post.likes > 3\n    )\n  ");
    expect(astToSqlWhere(ast)).toEqual(sql);
});
test("astToSqlWhere: Handles relational clauses (2)", function () {
    var filters = {
        AND: [
            { createdAt: { gt: "2028-2-2" } },
            {
                posts: {
                    some: {
                        likes: { gt: 3 },
                    },
                },
            },
        ],
    };
    var ast = {
        AND: [
            { clause: "table.createdat > ?", variables: ["2028-2-2"] },
            {
                joins: function (where) { return "user.inkey in (\n          SELECT user.inkey FROM user\n            INNER JOIN post ON post.authorid = user.id\n          WHERE " + where + "\n        )"; },
                subClause: { clause: "post.likes > ?", variables: [3] },
            },
        ],
    };
    var sql = sqlFormatter.format("(\n    table.createdat > '2028-2-2'\n    AND user.inkey in (\n      SELECT user.inkey FROM user\n        INNER JOIN post ON post.authorid = user.id\n      WHERE post.likes > 3\n    )\n  )");
    expect(astToSqlWhere(ast)).toEqual(sql);
});
test("astToSqlWhere: Handles relational clauses (3)", function () {
    var filters = {
        AND: [
            { createdAt: { gt: "2028-2-2" } },
            {
                posts: {
                    some: {
                        likes: { gt: 3 },
                        favorites: { gt: 4 },
                    },
                },
            },
        ],
    };
    var ast = {
        AND: [
            { clause: "table.createdat > ?", variables: ["2028-2-2"] },
            {
                joins: function (where) { return "user.inkey in (\n          SELECT user.inkey FROM user\n            INNER JOIN post ON post.authorid = user.id\n          WHERE " + where + "\n        )"; },
                subClause: {
                    AND: [
                        { clause: "post.likes > ?", variables: [3] },
                        { clause: "post.favorites > ?", variables: [4] },
                    ],
                },
            },
        ],
    };
    var sql = sqlFormatter.format("(\n    table.createdat > '2028-2-2'\n    AND user.inkey in (\n      SELECT user.inkey FROM user\n        INNER JOIN post ON post.authorid = user.id\n      WHERE (\n        post.likes > 3\n        AND post.favorites > 4\n      )\n    )\n  )");
    expect(astToSqlWhere(ast)).toEqual(sql);
});
test("astToSqlWhere: Handles combination of relational clauses", function () {
    var filters = {
        AND: [
            { createdAt: { gt: "2028-2-2" } },
            {
                posts: {
                    OR: [
                        {
                            some: {
                                likes: { gt: 3 },
                                favorites: { gt: 4 },
                            },
                        },
                        {
                            some: {
                                likes: { gt: 10 },
                                favorites: { gt: 1 },
                            },
                        },
                    ],
                },
            },
        ],
    };
    var ast = {
        AND: [
            { clause: "table.createdat > ?", variables: ["2028-2-2"] },
            {
                OR: [
                    {
                        joins: function (where) { return "user.inkey in (\n              SELECT user.inkey FROM user\n                INNER JOIN post ON post.authorid = user.id\n              WHERE " + where + "\n            )"; },
                        subClause: {
                            AND: [
                                { clause: "post.likes > ?", variables: [3] },
                                { clause: "post.favorites > ?", variables: [4] },
                            ],
                        },
                    },
                    {
                        joins: function (where) { return "user.inkey in (\n              SELECT user.inkey FROM user\n                INNER JOIN post ON post.authorid = user.id\n              WHERE " + where + "\n            )"; },
                        subClause: {
                            AND: [
                                { clause: "post.likes > ?", variables: [10] },
                                { clause: "post.favorites > ?", variables: [1] },
                            ],
                        },
                    },
                ],
            },
        ],
    };
    var sql = sqlFormatter.format("(\n    table.createdat > '2028-2-2'\n    AND (\n      user.inkey in (\n        SELECT user.inkey FROM user\n          INNER JOIN post ON post.authorid = user.id\n        WHERE (\n          post.likes > 3\n          AND post.favorites > 4\n        )\n      )\n      OR user.inkey in (\n        SELECT user.inkey FROM user\n          INNER JOIN post ON post.authorid = user.id\n        WHERE (\n          post.likes > 10\n          AND post.favorites > 1\n        )\n      )\n    )\n  )");
    expect(astToSqlWhere(ast)).toEqual(sql);
});
test("astToSqlWhere: Handles many-to-one relations (no some/all/none notation)", function () {
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
    var sql = "\n    \"post-alias\".id IN (\n      SELECT \"post\".id FROM \"post\"\n        INNER JOIN \"user\" ON \"user\".id = \"post\".userid\n      WHERE \"user\".postcount > 3\n    )\n  ";
    expect(formatSql(astToSqlWhere(ast))).toEqual(formatSql(sql));
});
test("astToSqlWhere: test", function () {
    var ast = {
        NOT: {
            AND: [
                { clause: "table.createdAt > ?", variables: ["2028-2-2"] },
                {
                    OR: [
                        { clause: "table.cost < ?", variables: [3] },
                        {
                            AND: [
                                { clause: "table.id in (?, ?, ?, ?)", variables: [2, 5, 8, 9] },
                                { clause: "table.cost > ?", variables: [20] },
                            ],
                        },
                    ],
                },
            ],
        },
    };
    var sql = sqlFormatter.format("\n  ( NOT\n    (\n      table.createdAt > '2028-2-2'\n      AND (\n        table.cost < 3\n        OR (\n          table.id in (2,5,8,9)\n          AND table.cost > 20\n        )\n      )\n    )\n  )\n");
    expect(astToSqlWhere(ast)).toEqual(sql);
});
//# sourceMappingURL=astToSqlWhere.test.js.map