const R = require("ramda")

const formatSql = require("sql-formatter").format
const { formatFilters } = require("./formatFilters")
const { astToSqlWhere } = require("./astToSqlWhere")
const { formattedFiltersToWhereAST } = require("./formattedFiltersToWhereAST")

const schema = {
  _typeMap: {
    User: {
      _typeConfig: {
        sqlTable: `"user"`,
        uniqueKey: "id",
      },
      _fields: {
        postCount: { sqlColumn: "postcount" },
        firstName: { sqlColumn: "firstname" },
        accounts: { sqlColumn: "accounts" },
        email: { sqlColumn: "email" },

        clubs: {
          junction: {
            returnType: "Club",
            sqlTable: `"member"`,
            uniqueKey: "id",
            sqlBatch: {
              parentKey: "id",
              thisKey: "userid",
              sqlJoin: (junction, payment) =>
                `${junction}.clubid = ${payment}.id`,
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
        sqlTable: `"post"`,
        uniqueKey: "id",
      },
      _fields: {
        title: { sqlColumn: "titlecolumn" },
        content: { sqlColumn: "contentcolumn" },
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
        sqlTable: `"club"`,
        uniqueKey: "id",
      },
      _fields: {
        name: { sqlColumn: "namecolumn" },
        stars: { sqlColumn: "stars" },

        users: {
          junction: {
            returnType: "User",
            uniqueKey: "id",
            sqlTable: `"club-user"`,
            sqlBatch: {
              thisKey: "clubid",
              parentKey: "id",
              sqlJoin: (junction, user) => `${junction}.userid = ${user}.id`,
            },
          },
        },
      },
    },

    Meh: {
      _typeConfig: {
        sqlTable: `"meh"`,
        uniqueKey: "id",
      },
      _fields: {
        z: { sqlColumn: "z" },
        y: { sqlColumn: "y" },
        x: { sqlColumn: "x" },
      },
    },
  },
}

test("formatFilters: Leave single filters alone", () => {
  const filters = {
    z: { gt: 1 },
  }

  const formattedFilters = {
    z: { gt: 1 },
  }

  expect(formatFilters(filters, { schema, type: "Meh" })).toEqual(
    formattedFilters
  )
})

test("formatFilters: Nested ANDs in ANDs get flattened (1)", () => {
  const filters = {
    z: { gt: 1 },
    AND: [{ x: { gt: 3 } }, { y: { lt: 8 } }],
  }

  const formattedFilters = {
    AND: [{ z: { gt: 1 } }, { x: { gt: 3 } }, { y: { lt: 8 } }],
  }

  expect(formatFilters(filters, { schema, type: "Meh" })).toEqual(
    formattedFilters
  )
})

test("formatFilters: Nested ANDs in ANDs get flattened (2)", () => {
  const filters = {
    AND: [
      { x: { gt: "2028-2-2" } },
      {
        AND: [
          { y: { lt: 3, gt: 7, in: [1, 2, 3, 4, 5] } },
          { y: { gt: 7 } },
          { x: { lt: "2018-2-2" } },
        ],
      },
    ],
  }

  const formattedFilters = {
    AND: [
      { x: { gt: "2028-2-2" } },
      { y: { lt: 3 } },
      { y: { gt: 7 } },
      { y: { in: [1, 2, 3, 4, 5] } },
      { y: { gt: 7 } },
      { x: { lt: "2018-2-2" } },
    ],
  }

  expect(formatFilters(filters, { schema, type: "Meh" })).toEqual(
    formattedFilters
  )
})

test("formatFilters: Nested ORs in ORs get flattened", () => {
  const filters = {
    OR: [{ z: { gt: 1 } }, { OR: [{ x: { gt: 3 } }, { y: { lt: 8 } }] }],
  }

  const formattedFilters = {
    OR: [{ z: { gt: 1 } }, { x: { gt: 3 } }, { y: { lt: 8 } }],
  }

  expect(formatFilters(filters, { schema, type: "Meh" })).toEqual(
    formattedFilters
  )
})

test("formatFilters: Single ORs in ANDs get flattened (1)", () => {
  const filters = {
    OR: [{ x: { gt: 3 } }],
  }

  const formattedFilters = {
    x: { gt: 3 },
  }

  expect(formatFilters(filters, { schema, type: "Meh" })).toEqual(
    formattedFilters
  )
})

test("formatFilters: Single ORs in ANDs get flattened (2)", () => {
  const filters = {
    AND: [
      { z: { gt: 2 } },
      {
        OR: [{ x: { gt: 3 } }],
      },
    ],
  }

  const formattedFilters = {
    AND: [{ z: { gt: 2 } }, { x: { gt: 3 } }],
  }

  expect(formatFilters(filters, { schema, type: "Meh" })).toEqual(
    formattedFilters
  )
})

test("formatFilters: flattens multiple layers", () => {
  const filters = {
    OR: [
      {
        AND: [{ x: { gt: 3 } }],
      },
    ],
  }

  const formattedFilters = {
    x: { gt: 3 },
  }

  expect(formatFilters(filters, { schema, type: "Meh" })).toEqual(
    formattedFilters
  )
})

test("formatFilters: Handle multi condition objects", () => {
  const filters = {
    x: { gt: "2028-2-2" },
    y: { lt: 3, gt: 7, in: [1, 2, 3, 4, 5] },
  }

  const formattedFilters = {
    AND: [
      { x: { gt: "2028-2-2" } },
      { y: { lt: 3 } },
      { y: { gt: 7 } },
      { y: { in: [1, 2, 3, 4, 5] } },
    ],
  }

  expect(formatFilters(filters, { schema, type: "Meh" })).toEqual(
    formattedFilters
  )
})

test("formatFilters: Complex (1)", () => {
  const filters = {
    x: { gt: "2028-2-2" },
    OR: [
      { y: { gt: 3 } },
      { z: { gt: 3, lt: 5 } },
      { OR: [{ z: { gt: 3 } }] },
      {
        x: { in: [2, 5, 8, 9] },
        z: { gt: 20 },
      },
    ],
  }

  const filtersFormatted = {
    AND: [
      { x: { gt: "2028-2-2" } },
      {
        OR: [
          { y: { gt: 3 } },
          {
            AND: [{ z: { gt: 3 } }, { z: { lt: 5 } }],
          },
          { z: { gt: 3 } },
          {
            AND: [{ x: { in: [2, 5, 8, 9] } }, { z: { gt: 20 } }],
          },
        ],
      },
    ],
  }

  expect(formatFilters(filters, { schema, type: "Meh" })).toEqual(
    filtersFormatted
  )
})

test("formatFilters: Handles relational filters (1)", () => {
  const filters = {
    clubs: {
      SOME: {
        name: { like: "%A", regex: "^B.*" },
      },
    },
  }

  const filtersFormatted = {
    clubs: {
      SOME: {
        AND: [{ name: { like: "%A" } }, { name: { regex: "^B.*" } }],
      },
    },
  }

  expect(formatFilters(filters, { schema, type: "User" })).toEqual(
    filtersFormatted
  )
})

test("formatFilters: Handles relational filters (2)", () => {
  const filters = {
    clubs: {
      SOME: {
        name: { like: "%A", regex: "^B.*" },
      },
      ALL: {
        stars: { gt: 3, lt: 9 },
      },
    },
  }

  const filtersFormatted = {
    AND: [
      {
        clubs: {
          SOME: {
            AND: [{ name: { like: "%A" } }, { name: { regex: "^B.*" } }],
          },
        },
      },
      {
        clubs: {
          ALL: {
            AND: [{ stars: { gt: 3 } }, { stars: { lt: 9 } }],
          },
        },
      },
    ],
  }

  expect(formatFilters(filters, { schema, type: "User" })).toEqual(
    filtersFormatted
  )
})

test("formatFilters: Handles relational filters (2)", () => {
  const filters = {
    posts: {
      SOME: {
        title: { regex: "^B.*" },
        content: { like: "^A%", regex: "^B.*" },
      },
    },
  }

  const filtersFormatted = {
    posts: {
      SOME: {
        AND: [
          { title: { regex: "^B.*" } },
          { content: { like: "^A%" } },
          { content: { regex: "^B.*" } },
        ],
      },
    },
  }

  expect(formatFilters(filters, { schema, type: "User" })).toEqual(
    filtersFormatted
  )
})

test("formatFilters: handles multiple relational filters", () => {
  const filters = {
    stars: { gt: 0 },
    users: {
      SOME: {
        postCount: { gt: 1 },
        firstName: { regex: "^B.*" },
      },
      ALL: {
        firstName: { regex: "^A.*", like: "%B" },
        postCount: { lt: 9 },
      },
    },
  }

  const filtersFormatted = {
    AND: [
      { stars: { gt: 0 } },
      {
        users: {
          SOME: {
            AND: [{ postCount: { gt: 1 } }, { firstName: { regex: "^B.*" } }],
          },
        },
      },
      {
        users: {
          ALL: {
            AND: [
              { firstName: { regex: "^A.*" } },
              { firstName: { like: "%B" } },
              { postCount: { lt: 9 } },
            ],
          },
        },
      },
    ],
  }

  expect(formatFilters(filters, { schema, type: "Club" })).toEqual(
    filtersFormatted
  )
})

test("formatFilters: complex relational", () => {
  const filters = {
    stars: { gt: 5 },
    users: {
      SOME: {
        firstName: { regex: "^A.*", like: "%B" },
        postCount: { gt: 5 },
      },
      OR: [
        {
          AND: [
            {
              ALL: {
                accounts: { gt: 1, lt: 3 },
              },
            },
            {
              SOME: {
                accounts: { gt: 2 },
              },
            },
          ],
        },
        {
          NONE: {
            accounts: { gt: 1, lt: 3 },
          },
        },
      ],
    },
  }

  const filtersFormatted = {
    AND: [
      { stars: { gt: 5 } },
      {
        users: {
          SOME: {
            AND: [
              { firstName: { regex: "^A.*" } },
              { firstName: { like: "%B" } },
              { postCount: { gt: 5 } },
            ],
          },
        },
      },
      {
        OR: [
          {
            AND: [
              {
                users: {
                  ALL: {
                    AND: [{ accounts: { gt: 1 } }, { accounts: { lt: 3 } }],
                  },
                },
              },
              {
                users: {
                  SOME: {
                    accounts: { gt: 2 },
                  },
                },
              },
            ],
          },
          {
            users: {
              NONE: {
                AND: [{ accounts: { gt: 1 } }, { accounts: { lt: 3 } }],
              },
            },
          },
        ],
      },
    ],
  }

  expect(formatFilters(filters, { schema, type: "Club" })).toEqual(
    filtersFormatted
  )
})

test("formatFilters: complex relational full", () => {
  const filters = {
    stars: { gt: 5 },
    users: {
      SOME: {
        firstName: { regex: "^A.*", like: "%B" },
        postCount: { gt: 5 },
      },
      OR: [
        {
          AND: [
            {
              ALL: {
                accounts: { gt: 1, lt: 3 },
              },
            },
            {
              SOME: {
                accounts: { gt: 2 },
              },
            },
          ],
        },
        {
          NONE: {
            accounts: { gt: 1, lt: 3 },
          },
        },
      ],
    },
  }

  const sql = formatSql(`(
    "club-alias".stars > 5
    AND "club-alias".id IN (
      SELECT "club".id FROM "club"
      INNER JOIN "club-user" ON "club-user".clubid = "club".id
      INNER JOIN "user" ON "club-user".userid = "user".id
      WHERE (
        "user".firstname ~ '^A.*'
        AND "user".firstname LIKE '%B'
        AND "user".postcount > 5
      )
    )
    AND (
      (
        "club-alias".id NOT IN (
          SELECT "club".id FROM "club"
          INNER JOIN "club-user" ON "club-user".clubid = "club".id
          INNER JOIN "user" ON "club-user".userid = "user".id
          WHERE NOT (
            "user".accounts > 1
            AND "user".accounts < 3
          )
        )
        AND "club-alias".id IN (
          SELECT "club".id FROM "club"
          INNER JOIN "club-user" ON "club-user".clubid = "club".id
          INNER JOIN "user" ON "club-user".userid = "user".id
          WHERE "user".accounts > 2
        )
      )
      OR "club-alias".id NOT IN (
        SELECT "club".id FROM "club"
        INNER JOIN "club-user" ON "club-user".clubid = "club".id
        INNER JOIN "user" ON "club-user".userid = "user".id
        WHERE (
          "user".accounts > 1
          AND "user".accounts < 3
        )
      )
    )
  )`)

  expect(
    astToSqlWhere(
      formattedFiltersToWhereAST(
        formatFilters(filters, { schema, type: "Club" }),
        { schema, type: "Club", tableName: '"club-alias"' }
      )
    )
  ).toEqual(sql)
})

test("formatFilters: Handle many-to-one (no need for some/all/none notation)", () => {
  const filters = {
    author: {
      email: { eq: "test@email.com" },
    },
  }

  const formattedFilters = {
    author: {
      email: { eq: "test@email.com" },
    },
  }

  expect(
    formatFilters(filters, { schema, type: "Post", tableName: '"post-alias"' })
  ).toEqual(formattedFilters)
})

test("formatFilters: handles 'NOT' direct syntax", () => {
  const filters = {
    content: { NOT: { gt: 3 } },
  }

  const formattedFilters = {
    NOT: { content: { gt: 3 } },
  }

  expect(
    formatFilters(filters, { schema, type: "Post", tableName: '"post-alias"' })
  ).toEqual(formattedFilters)
})

test("formatFilters: handles 'NOT' block syntax?", () => {
  const filters = {
    NOT: { content: { gt: 3 } },
  }

  const formattedFilters = {
    NOT: { content: { gt: 3 } },
  }

  expect(
    formatFilters(filters, { schema, type: "Post", tableName: '"post-alias"' })
  ).toEqual(formattedFilters)
})

test("formatFilters: handles 'NOT' (2)", () => {
  const filters = {
    NOT: {
      content: {
        gt: 3,
        lt: 9,
      },
    },
  }

  const formattedFilters = {
    NOT: {
      AND: [{ content: { gt: 3 } }, { content: { lt: 9 } }],
    },
  }

  expect(
    formatFilters(filters, { schema, type: "Post", tableName: '"post-alias"' })
  ).toEqual(formattedFilters)
})

test("formatFilters: handles 'NOT' (3)", () => {
  const filters = {
    content: {
      NOT: {
        gt: 3,
        lt: 9,
      },
    },
  }

  const formattedFilters = {
    NOT: {
      AND: [{ content: { gt: 3 } }, { content: { lt: 9 } }],
    },
  }

  expect(
    formatFilters(filters, { schema, type: "Post", tableName: '"post-alias"' })
  ).toEqual(formattedFilters)
})
