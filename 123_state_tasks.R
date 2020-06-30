do_state_tasks <- function(oldest_active_sites, ...) {

  split_inventory(summary_file='1_fetch/tmp/state_splits.yml',
                  sites_info=oldest_active_sites)

  # Define task table rows
  state_abbv <- oldest_active_sites$state_cd

  # Define task table columns
  download_step <- create_task_step(
    step_name = 'download',
    target_name = function(task_name, step_name, ...) {
      sprintf("%s_data", task_name)
    },
    command = function(..., task_name) {
      sprintf("get_site_data(state_info_file='1_fetch/tmp/inventory_%s.tsv', parameter=parameter)", task_name)
    }
  )

  # Create the task plan
  task_plan <- create_task_plan(
    task_names = state_abbv,
    task_steps = list(download_step),
    add_complete = FALSE)

  # Create the task remakefile
  create_task_makefile(
    task_plan = task_plan,
    makefile = '123_state_tasks.yml',
    include = 'remake.yml',
    sources = '1_fetch/src/get_site_data.R',
    packages = c('tidyverse', 'dataRetrieval'),
    tickquote_combinee_objects = FALSE,
    finalize_funs = c())

  # Build the tasks
  scmake('123_state_tasks', remake_file='123_state_tasks.yml')

  # Return nothing to the parent remake file
  return()
}

split_inventory <- function(
  summary_file='1_fetch/tmp/state_splits.yml',
  sites_info=oldest_active_sites) {

  if(!dir.exists('1_fetch/tmp')) dir.create('1_fetch/tmp')

  # Loop over states and save info to separate files
  split_files <- purrr::map(sites_info$state_cd, function(state) {
    state_fn <- sprintf("1_fetch/tmp/inventory_%s.tsv", state)
    readr::write_tsv(
      filter(sites_info, state_cd == state),
      path = state_fn)
    return(state_fn)
  }) %>%
    purrr::reduce(c) %>%
    sort()

  sc_indicate(ind_file = summary_file, data_file = split_files)
}
