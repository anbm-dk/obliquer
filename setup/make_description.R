# Make description

library(desc)
library(usethis)

use_mit_license()

x <- desc(file = "DESCRIPTION")

# Title and description

x$set(
  Package = "obliquer",
  Title = "Oblique geographic coordinates for spatial predictions",
  Description = "Methods to generate oblique geographic coordinates for spatial predictions with machine learning models.",
  URL = "https://github.com/anbm-dk/obliquer",
  BugReports = "https://github.com/anbm-dk/obliquer/issues"
)

# Set version

x$set_version("0.1")

# Set dependencies

x$set_dep("terra")
x$set_dep("magrittr")
x$set_dep("foreach")

# Set authors

x$del("Authors@R")

x$add_author(
  given = "Anders Bjørn",
  family = "Møller",
  email = "anbm@agro.au.dk",
  orcid = "0000-0002-2737-5780",
  role = c("aut", "cre"),
)

x$get_authors()

# Last check

x

# Write description

x$write(file = "DESCRIPTION")


# END
