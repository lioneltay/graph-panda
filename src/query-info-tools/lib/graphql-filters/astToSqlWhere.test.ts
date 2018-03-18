const R = require("ramda")
const sqlFormatter = require("sql-formatter")
const formatSql = require("sql-formatter").format

const { astToSqlWhere } = require("./astToSqlWhere")

test("astToSqlWhere: Handles single clause", () => {
  const ast = {
    clause: `table.createdAt > ?`,
    variables: ["2028-2-2"],
  }

  const sql = sqlFormatter.format(`
    table.createdAt > '2028-2-2'
  `)

  expect(astToSqlWhere(ast)).toEqual(sql)
})

test("astToSqlWhere: test", () => {
  const ast = {
    AND: [
      { clause: `table.createdAt > ?`, variables: ["2028-2-2"] },
      {
        OR: [
          { clause: `table.cost < ?`, variables: [3] },
          {
            AND: [
              { clause: `table.id in (?, ?, ?, ?)`, variables: [2, 5, 8, 9] },
              { clause: `table.cost > ?`, variables: [20] },
            ],
          },
        ],
      },
    ],
  }

  const sql = sqlFormatter.format(`
  (
    table.createdAt > '2028-2-2'
    AND (
      table.cost < 3
      OR (
        table.id in (2,5,8,9)
        AND table.cost > 20
      )
    )
  )
`)

  expect(astToSqlWhere(ast)).toEqual(sql)
})

test("astToSqlWhere: test 2 complex", () => {
  const ast = {
    AND: [
      { clause: `table.createdAt > ?`, variables: ["2028-2-2"] },
      {
        OR: [
          { clause: `table.cost < ?`, variables: [3] },
          {
            AND: [
              { clause: `table.id in (?, ?, ?, ?)`, variables: [2, 5, 8, 9] },
              { clause: `table.cost > ?`, variables: [20] },
              {
                AND: [
                  { clause: `table.cost > ?`, variables: [20] },
                  { clause: `table.cost < ?`, variables: [30] },
                ],
              },
            ],
          },
        ],
      },
    ],
  }

  const sql = sqlFormatter.format(`
  (
    table.createdAt > '2028-2-2'
    AND (
      table.cost < 3
      OR (
        table.id in (2,5,8,9)
        AND table.cost > 20
        AND (
          table.cost > 20
          AND table.cost < 30
        )
      )
    )
  )
`)

  expect(astToSqlWhere(ast)).toEqual(sql)
})

test("astToSqlWhere: test 3 complex", () => {
  const ast = {
    AND: [
      { clause: `table.createdAt > ?`, variables: ["2028-2-2"] },
      {
        OR: [
          { clause: `table.cost < ?`, variables: [3] },
          {
            AND: [
              { clause: `table.id in (?, ?, ?, ?)`, variables: [2, 5, 8, 9] },
              { clause: `table.cost > ?`, variables: [20] },
              {
                OR: [
                  { clause: `table.name like ?`, variables: ["A%"] },
                  { clause: `table.name regex ?`, variables: [".*A^"] },
                ],
              },
              {
                AND: [
                  { clause: `table.cost > ?`, variables: [20] },
                  { clause: `table.cost < ?`, variables: [30] },
                ],
              },
            ],
          },
        ],
      },
    ],
  }

  const sql = sqlFormatter.format(`
    (
      table.createdAt > '2028-2-2'
      AND (
        table.cost < 3
        OR (
          table.id in (2,5,8,9)
          AND table.cost > 20
          AND (
            table.name like 'A%'
            OR table.name regex '.*A^'
          )
          AND (
            table.cost > 20
            AND table.cost < 30
          )
        )
      )
    )
  `)

  expect(astToSqlWhere(ast)).toEqual(sql)
})

test("astToSqlWhere: Handles relational clauses (1)", () => {
  const filters = {
    posts: {
      some: {
        likes: { gt: 3 },
      },
    },
  }

  const ast = {
    joins: where => `user.inkey in (
      SELECT user.inkey FROM user
        INNER JOIN post ON post.authorid = user.id
      WHERE ${where}
    )`,
    subClause: { clause: `post.likes > ?`, variables: [3] },
  }

  const sql = sqlFormatter.format(`
    user.inkey in (
      SELECT user.inkey FROM user
        INNER JOIN post ON post.authorid = user.id
      WHERE post.likes > 3
    )
  `)

  expect(astToSqlWhere(ast)).toEqual(sql)
})

test("astToSqlWhere: Handles relational clauses (2)", () => {
  const filters = {
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
  }

  const ast = {
    AND: [
      { clause: `table.createdat > ?`, variables: ["2028-2-2"] },
      {
        joins: where => `user.inkey in (
          SELECT user.inkey FROM user
            INNER JOIN post ON post.authorid = user.id
          WHERE ${where}
        )`,
        subClause: { clause: `post.likes > ?`, variables: [3] },
      },
    ],
  }

  const sql = sqlFormatter.format(`(
    table.createdat > '2028-2-2'
    AND user.inkey in (
      SELECT user.inkey FROM user
        INNER JOIN post ON post.authorid = user.id
      WHERE post.likes > 3
    )
  )`)

  expect(astToSqlWhere(ast)).toEqual(sql)
})

test("astToSqlWhere: Handles relational clauses (3)", () => {
  const filters = {
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
  }

  const ast = {
    AND: [
      { clause: `table.createdat > ?`, variables: ["2028-2-2"] },
      {
        joins: where => `user.inkey in (
          SELECT user.inkey FROM user
            INNER JOIN post ON post.authorid = user.id
          WHERE ${where}
        )`,
        subClause: {
          AND: [
            { clause: `post.likes > ?`, variables: [3] },
            { clause: `post.favorites > ?`, variables: [4] },
          ],
        },
      },
    ],
  }

  const sql = sqlFormatter.format(`(
    table.createdat > '2028-2-2'
    AND user.inkey in (
      SELECT user.inkey FROM user
        INNER JOIN post ON post.authorid = user.id
      WHERE (
        post.likes > 3
        AND post.favorites > 4
      )
    )
  )`)

  expect(astToSqlWhere(ast)).toEqual(sql)
})
test("astToSqlWhere: Handles combination of relational clauses", () => {
  const filters = {
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
  }

  const ast = {
    AND: [
      { clause: `table.createdat > ?`, variables: ["2028-2-2"] },
      {
        OR: [
          {
            joins: where => `user.inkey in (
              SELECT user.inkey FROM user
                INNER JOIN post ON post.authorid = user.id
              WHERE ${where}
            )`,
            subClause: {
              AND: [
                { clause: `post.likes > ?`, variables: [3] },
                { clause: `post.favorites > ?`, variables: [4] },
              ],
            },
          },
          {
            joins: where => `user.inkey in (
              SELECT user.inkey FROM user
                INNER JOIN post ON post.authorid = user.id
              WHERE ${where}
            )`,
            subClause: {
              AND: [
                { clause: `post.likes > ?`, variables: [10] },
                { clause: `post.favorites > ?`, variables: [1] },
              ],
            },
          },
        ],
      },
    ],
  }

  const sql = sqlFormatter.format(`(
    table.createdat > '2028-2-2'
    AND (
      user.inkey in (
        SELECT user.inkey FROM user
          INNER JOIN post ON post.authorid = user.id
        WHERE (
          post.likes > 3
          AND post.favorites > 4
        )
      )
      OR user.inkey in (
        SELECT user.inkey FROM user
          INNER JOIN post ON post.authorid = user.id
        WHERE (
          post.likes > 10
          AND post.favorites > 1
        )
      )
    )
  )`)

  expect(astToSqlWhere(ast)).toEqual(sql)
})

test("astToSqlWhere: Handles many-to-one relations (no some/all/none notation)", () => {
  const formattedFilters = {
    author: {
      postCount: { gt: 3 },
    },
  }

  const ast = {
    joins: where => `"post-alias".id IN (
      SELECT "post".id FROM "post"
        INNER JOIN "user" ON "user".id = "post".userid
      WHERE ${where}
    )`,
    subClause: {
      clause: `"user".postcount > ?`,
      variables: [3],
    },
  }

  const sql = `
    "post-alias".id IN (
      SELECT "post".id FROM "post"
        INNER JOIN "user" ON "user".id = "post".userid
      WHERE "user".postcount > 3
    )
  `

  expect(formatSql(astToSqlWhere(ast))).toEqual(formatSql(sql))
})

test("astToSqlWhere: test", () => {
  const ast = {
    NOT: {
      AND: [
        { clause: `table.createdAt > ?`, variables: ["2028-2-2"] },
        {
          OR: [
            { clause: `table.cost < ?`, variables: [3] },
            {
              AND: [
                { clause: `table.id in (?, ?, ?, ?)`, variables: [2, 5, 8, 9] },
                { clause: `table.cost > ?`, variables: [20] },
              ],
            },
          ],
        },
      ],
    },
  }

  const sql = sqlFormatter.format(`
  ( NOT
    (
      table.createdAt > '2028-2-2'
      AND (
        table.cost < 3
        OR (
          table.id in (2,5,8,9)
          AND table.cost > 20
        )
      )
    )
  )
`)

  expect(astToSqlWhere(ast)).toEqual(sql)
})
