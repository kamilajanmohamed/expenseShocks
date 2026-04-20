#############################################################
# Author: Kamila Janmohamed
# Date: 2026-03-18
# Description: Configure file paths for project
#############################################################

# Set paths ------------------------------------------------------------
# Derive root from this file's own location — works for any user/machine.
# config.R lives at <root>/code/00_setup/config.R, so go up three levels.
# sys.frame(1)$ofile works when sourced; fall back to rstudioapi for
# interactive line-by-line execution in RStudio.
this_file <- tryCatch(
  normalizePath(sys.frame(1)$ofile, mustWork = FALSE),
  error = function(e)
    normalizePath(rstudioapi::getActiveDocumentContext()$path, mustWork = FALSE)
)
root <- dirname(dirname(this_file))  # ExpenseShocks/

# Set working directory to root so relative paths (data/, code/) resolve correctly
setwd(root)