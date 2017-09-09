DDL Diff
========

This is a tool to generate database schema migrations from changes to `create` statements.
It parses SQL Data Definition Language (DDL) statements for two versions of the same table, diffs the ASTs and generates a sensible data migration from A to B.

This allows you to track changes to your schema as changes to a single file, which will play nice with version control.

e.g.

```sql
;; file: foo.table.sql @ version 213af1de
create table Foo (id number);
```

```sql
;; file: foo.table.sql @ version 9bb4545e
create table Foo (id number, name text);
```

==> 

```sql
;; file: foo.table.migration.213af1de-9bb4545e.sql (or something)
alter table Foo add column (name text);
```

Bad news
--------

This is just an experiment! I don't even know Scala (yet)!

It also can't ever 100% work in principle - only the user will know exactly how to migrate the data, and whether data migrations need to be interleaved with schema migrations. Cross that bridge later...
