EmacSQL is a high-level Emacs Lisp front-end for SQLite
(primarily), PostgreSQL, MySQL, and potentially other SQL
databases.

During package installation EmacSQL will attempt to compile a
custom native binary for communicating with a SQLite database. If
this fails (a C compiler is not available), it will attempt to
download, with permission, a pre-built binary when the first
database connection is attempted.

Most EmacSQL functions operate on a database connection. A
connection to SQLite is established with `emacsql-connect'. For
each such connection a sqlite3 inferior process is kept alive in
the background. Connections are closed with `emacsql-close'.

    (defvar db (emacsql-connect "company.db"))

Other types of database connections are available (PostgreSQL via
`emacsql-psql').

Use `emacsql' to send an s-expression SQL statements to a connected
database. Identifiers for tables and columns are symbols. SQL
keywords are lisp keywords. Anything else is data.

    (emacsql db [:create-table people ([name id salary])])

Column constraints can optionally be provided in the schema.

    (emacsql db [:create-table people ([name (id integer :unique) salary])])

Insert some values.

    (emacsql db [:insert :into people
                 :values (["Jeff"  1000 60000.0] ["Susan" 1001 64000.0])])

Currently all actions are synchronous and Emacs will block until
SQLite has indicated it is finished processing the last command.

Query the database for results:

    (emacsql db [:select [name id] :from employees :where (> salary 60000)])
    ;; => (("Susan" 1001))

Queries can be templates -- $i1, $s2, etc. -- so they don't need to
be built up dynamically:

    (emacsql db
             [:select [name id] :from employees :where (> salary $s1)]
             50000)
    ;; => (("Jeff" 1000) ("Susan" 1001))

The letter declares the type (identifier, scalar, vector, Schema)
and the number declares the argument position.

See README.md for much more complete documentation.
