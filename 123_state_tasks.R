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
    final_steps = c('tally', 'plot'),
    add_complete = FALSE)

  # Create the task remakefile
  create_task_makefile(
    task_plan = task_plan,
    makefile = '123_state_tasks.yml',
    include = 'remake.yml',
    sources = c(...),
    packages = c('tidyverse', 'dataRetrieval', 'lubridate'),
    final_targets = c('obs_tallies', '3_visualize/out/timeseries_plots.yml'),
    finalize_funs = c('combine_obs_tallies', 'summarize_timeseries_plots'),
    as_promises = TRUE,
    tickquote_combinee_objects = TRUE)

  # Build the tasks
  loop_tasks(task_plan = task_plan,
             task_makefile = '123_state_tasks.yml',
             num_tries = 50)
  # copy `obs_tallies` to local environment since it was already built with `loop_tasks`
  obs_tallies <- remake::fetch('obs_tallies_promise', remake_file='123_state_tasks.yml')

  # Read timeseries_plots.yml into a tibble format (loop_tasks already made sure
  #   that `timeseries_plots.yml` was already built)
  timeseries_plots_info <- yaml::yaml.load_file('3_visualize/out/timeseries_plots.yml') %>%
    tibble::enframe(name = 'filename', value = 'hash') %>%
    mutate(hash = purrr::map_chr(hash, `[[`, 1))

  # Return the combiner targets to the parent remake file
  return(list(obs_tallies=obs_tallies, timeseries_plots_info=timeseries_plots_info))
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
  # filter to just those arguments that are tibbles (because the only step
  # outputs that are tibbles are the tallies)
  dots <- list(...)
  tally_dots <- dots[purrr::map_lgl(dots, is_tibble)]
  purrr::reduce(tally_dots, bind_rows)
}

summarize_timeseries_plots <- function(ind_file, ...) {
  # filter to just those arguments that are character strings (because the only
  # step outputs that are characters are the plot filenames)
  dots <- list(...)
  plot_dots <- dots[purrr::map_lgl(dots, is.character)]
  do.call(combine_to_ind, c(list(ind_file), plot_dots))
}
