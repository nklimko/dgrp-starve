library("workflowr")

getwd()
setwd("/data/morgante_lab/nklimko/rep")

# Configure Git (only need to do once per computer)
#wflow_git_config(user.name = "nklimko", user.email = "nklimko@clemson.edu")

# Start a new workflowr project
wflow_start("dgrp-starve", git = FALSE, existing = TRUE)

# Build the site
wflow_build()

# Customize your site!
#   1. Edit the R Markdown files in analysis/
#   2. Edit the theme and layout in analysis/_site.yml
#   3. Add new or copy existing R Markdown files to analysis/

# Preview your changes
wflow_build(update=TRUE)

?wflow_build()

wflow_status()



wflow_git_commit("/home/nklimko/R/stat/lab1/git-repoProof/code/flowR-walkthrough.R")
getwd()


wflow_git_commit(all = TRUE)


# Publish the site, i.e. version the source code and HTML results
wflow_publish("analysis/*", "Start my new project with R walkthrough added", republish = TRUE)



