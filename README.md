DDL Diff
========

This is a tool to generate database schema migrations from changes to `create` statements.
It parses SQL Data Definition Language (DDL) statements for two versions of the same table, diffs the ASTs and generates a sensible data migration from A to B.

This allows you to track changes to your schema as changes to a single file per database object, which will play nice with version control. So rather than having an initial definition and then a bunch of migrations stored in the repository you just keep and modify the initial definition, with the migrations generated automatically.

e.g. Imagine you had a table `foo` that was originally this:

```sql
-- file: foo.table.sql @ version 213af1de
create table Foo (id number);
```

but is now this:

```sql
-- file: foo.table.sql @ version 9bb4545e
create table Foo (id number, name text);
```

This tool will generate a migration like this:

```sql
-- file: foo.table.migration.213af1de-9bb4545e.sql (or something)
alter table Foo add column (name text);
```

We might then feed this output into something like [Flyway](https://flywaydb.org) to actually perform the migration.

Bad news
--------

- This is just an experiment - and not finished! I don't even know Scala (yet)!

- I'm starting with SQLite's SQL grammar because they great docs and it's easy to test. Will adapt to ANSI at some point, but will probably eventually need to implement multiple parsers.

- It can't ever 100% work in principle - only the user will know exactly how to migrate the data, and whether data migrations need to be interleaved with schema migrations. Cross that bridge later...
