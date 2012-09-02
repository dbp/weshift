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

Use of Digestive Functors:
in order to be able to produce forms without corresponding views defined where they are
used (ie, I want to be able to show a name change form without having set it up in
the handler that is rendering whatever template that has the name change form), we
use the same prefix everywhere and give defaults for the dfInput etc splices. 