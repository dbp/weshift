General Setup:
create a Secrets.hs file with pgUser, pgPassword, dbName, postmarkToken.
create corresponding database and user. 

Packages:
dbmigrations package at darcsden.com/dbp/dmigrations (fork with relaxed deps so it builds)
snap-less package at github.com/dbp/snap-less (fork with updated import for snap)
lessc in the path for LessCSS.

Database Setup:
you need to install postgres-contrib to get pgcrypto. 
install in database with 9.1 by running CREATE EXTENSION pgcrypto in the database

then set up dbmigrations and run the migrations that are in the migrations folder.