## Relational Filters

```javascript
filters: {
  eventTickets: {
    some: { // Property Filters
      price: {
        gt: 3
        lt: 50
      },
      name: {
        startsWith: 'ticket',
      }
      // Count(*) > 0
    },
    all: { // Property Filters
      ...propertyFilters
      // Count(filtered) === COUNT(all)
    },
    none: { // Property Filters
      ...propertyFilters
      // Count(*) === 0
    }
  }
}
```

## Problems


### Nested filter

```javascript
const filters = {
    // events?
    price: { gt: 10 },
    users: {
        // relational field
        some: {
            age: { gt: 3 },
        },
    },
}
```

Events that cost more than $10 and have at least 1 user who is age 3 or great

```sql
SELECT * FROM currentTable
WHERE (
  currentTable.price > 10
  AND (
    SELECT * FROM users
    LEFT JOIN currentTable
      ON currentTable.userid = users.id
    WHERE users.age > 3
  ) > 0
)
```

### Doubly nested filter

```javascript
const filters = { // events?
  price: { gt: 10 },
  users: { // relational field
    some: {
      age: { gt: 3 }
      post: { // nested relational field
        some: {
          title: { eq: 'the-title' }
        }
      }
    }
  }
}
```

Events that cost more than $10 and have at least 1 user who is age 3 or greater who has made a post with title 'the-title'

* potentially quite slow doing correlated subqueries
* also have to keep track of table aliases (hence correlated subquery)
```sql
SELECT * FROM currentTable
WHERE (
  currentTable.price > 10
  AND (
    SELECT * FROM user
    LEFT JOIN currentTable ON currentTable.userid = user.id
    WHERE (
      user.age > 3
      AND (
        SELECT * FROM post
        LEFT JOIN user ON user.postid = post.id
        WHERE post.title = 'the-title'
      ) > 0
    )
  ) > 0
)
```

* realy good performance by doing id in queries
* subqueries are not correlated
```sql
SELECT * FROM currentTable as aliasTable
WHERE (
  alias.price > 10
  AND alias.id in (
    SELECT currentTable.id FROM currentTable
      LEFT JOIN user ON currentTable.userid = user.id
    WHERE (
      user.age > 3
      AND user.id in (
        SELECT user.id FROM user
          LEFT JOIN post ON user.post = post.id
        WHERE post.title = 'the-title'
      )
    )
  )
)
```

## Model Filters are ok

```sql
select * from sometable
where (
  createdAt > '2018-1-1'
  AND (
    cost > 3
    OR (id in (1,5,8,9) AND cost > 10)
  )
)
```

```javascript
filters: {
  createdAt: { gt: '2018-1-1' },
  OR: [
    { cost: { gt: 3 } },
    {
      id: { in: [1,5,8,9] },
      cost: { gt: 10 }
    },
  ]
}
```


### types of relations
##### many to one
```javascript
const filters = {
  author: {
    userProp: { gt: 3 },
    id: { in: [1,2,3] }
  }
}
```

```sql
SELECT * FROM book
  WHERE book.id in (
    SELECT book.id FROM book
      INNER JOIN user ON book.authorId = user.id
    WHERE (
      user.userProp > 3
      AND user.id in (1, 2, 3)
    )
  )
```

* many to many
```javascript
const filters = {
  clubs: {
    some: {
      likes: { gt: 3 }
    }
  }
}
```

```sql
SELECT * FROM user
  WHERE user.id in (
    SELECT user.id FROM user
      INNER JOIN "user-club" ON user.id = "user-club".userid
      INNER JOIN club ON club.id = "user-club".clubid
    WHERE club.likes > 3
  )
```

* one to many
```javascript
const filters = {
  posts: {
    some: {
      likes: { gt: 3 }
    }
  }
}
```

```sql
SELECT * FROM user
  WHERE user.id in (
    SELECT user.id FROM user
      INNER JOIN post ON post.authorid = user.id
    WHERE post.likes > 3
  )
```

* common points
  * how to generate in condition
    * the parent tables unique key
  * how to make the join
    * which tables are involve
    * their join keys
  * the where conditions
    * same as others

###### symantics on some vs all vs none

all use inner joins
* some = in
* none = not in
* all = not in where not ( ...clauses )

###