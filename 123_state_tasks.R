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

  plot_step <- create_task_step(
    step_name = 'plot',
    target_name = function(task_name, ...) {
      sprintf("3_visualize/out/timeseries_%s.png", task_name)
    },
    command = function(steps, ...) {
      sprintf("plot_site_data(target_name, %s, parameter)",
              steps[["download"]]$target_name)
    }
  )

  tally_step <- create_task_step(
    step_name = "tally",
    target_name = function(task_name, ...) {
      sprintf("%s_tally", task_name)
    },
    command = function(steps, ...) {
      sprintf("tally_site_obs(%s)", steps[["download"]]$target_name)
    }
  )

  # Create the task plan
  task_plan <- create_task_plan(
    task_names = state_abbv,
    task_steps = list(download_step, plot_step, tally_step),
    final_steps = 'tally',
    add_complete = FALSE)

  # Create the task remakefile
  create_task_makefile(
    task_plan = task_plan,
    makefile = '123_state_tasks.yml',
    include = 'remake.yml',
    sources = c(...),
    packages = c('tidyverse', 'dataRetrieval', 'lubridate'),
    final_targets = 'obs_tallies',
    finalize_funs = 'combine_obs_tallies',
    as_promises = FALSE,
    tickquote_combinee_objects = TRUE)

  # Build the tasks
  obs_tallies <- scmake('obs_tallies', remake_file='123_state_tasks.yml')

  # Return the combined data frame of tallies from each state
  return(obs_tallies)
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

combine_obs_tallies <- function(...) {
  purrr::reduce(list(...), bind_rows)
}
