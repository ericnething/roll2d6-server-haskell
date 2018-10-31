#! bin/bash

cat scripts/reset.sql | psql -d postgres -U eric -h localhost
cat scripts/extensions.sql | psql -d faterpg -U eric -h localhost
cat scripts/init.sql | PGPASSWORD=faterpg psql -d faterpg -U faterpg -h localhost
