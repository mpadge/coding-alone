library(testthat)
library(longtail)

# fetch_issue_authors() calls progressr::handlers(global = TRUE) itself on
# every invocation. That registers a *global* calling handler
# (base::globalCallingHandlers()), which errors ("should not be called with
# handlers on the stack") if any calling handler is already active - which
# is always true once testthat starts running tests (it installs its own).
# Registering it once here - before test_check() ever sources/runs a single
# test file, so before any handler is on the stack - makes
# fetch_issue_authors()'s later calls a no-op: progressr's own registration
# logic checks whether its handler is already installed before adding it.
progressr::handlers (global = TRUE)

test_check("longtail")
