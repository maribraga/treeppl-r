#' Read JSON output from inference with TreePPL
#'
#' @param file_path Path to json file with the output from TreePPL inference.
#' @param sample_freq For each sweep, sample one particle every `sample_freq` particles.

#' @return A data frame with one row for each sample and columns for parameters and joint posterior probability.
#' @export
#'
#' @examples
#' \dontrun{
#'   # the common case is that read_treeppl_output is used within run_treeppl()
#'   # if you run treeppl outside of R, you can use this function to read in the output.
#'   logs <- read_treeppl_output("output/stdout.json", sample_freq = 1000)
#' }
read_treeppl_output <- function(file_path, sample_freq = 1000) {

  fromjson <- read_json_output(file_path)

  tidy_out <- tidy_output(fromjson) %>%
    lapply(FUN = function(x) dplyr::filter(x, .data$log_weights != "-Inf"))   # remove the last particle from smc-apf outputs

  out <- sub_sample_sweep(out, sample_freq)

  return(out)

}


read_json_output <- function(file_path) {

  # check if file has multiple lines/sweeps
  nlines <- system2("wc", c("-l", file_path), stdout = TRUE) %>%
    strex::str_first_number()

  if( nlines > 1 ){
    # amend json format
    system2(system.file("json_amend.zsh", package = "treepplr"), file_path)
  }

  # read file
  fromjson <- jsonlite::fromJSON(file_path)

  return(fromjson)

}


tidy_output <- function(fromjson) {

  # one or multiple sweeps?
  if(methods::is(fromjson, "list") & length(fromjson) == 3) {

    # only one sweep

    # one or more parameters?
    if(methods::is(fromjson$samples, "numeric")) {

      # one parameter
      out_list <- list(fromjson)
      tidy_list <- tidy_samples_vec(out_list)

    } else if(methods::is(fromjson$samples, "data.frame") & ncol(fromjson$samples) == 2){

      # several parameters
      out_list <- list()
      out_list[[1]] <- list(samples = fromjson$samples,
                            weights = fromjson$weights %>% unlist() %>% as.numeric(),
                            normConst = fromjson$normConst)
      tidy_list <- tidy_samples_df(out_list)

    }

  } else if(methods::is(fromjson, "data.frame") & ncol(fromjson) == 3) {

    # multiple sweeps

    # one or more parameters?
    if(methods::is(fromjson$samples[[1]], "numeric")) {

      # one parameter
      out_list <- list()
      for(i in seq_along(fromjson$samples)) {
        out_list[[i]] <- list(samples = fromjson$samples[[i]],
                              weights = fromjson$weights[[i]],
                              normConst = fromjson$normConst[i])
      }
      tidy_list <- tidy_samples_vec(out_list)

    } else if(methods::is(fromjson$samples[[1]], "data.frame") & ncol(fromjson$samples[[1]]) == 2){

      # several parameters
      out_list <- list()
      for(i in seq_along(fromjson$samples)) {
        out_list[[i]] <- list(samples = fromjson$samples[[i]],
                              weights = fromjson$weights[[i]] %>% unlist() %>% as.numeric(),
                              normConst = fromjson$normConst[i])
      }
      tidy_list <- tidy_samples_df(out_list)

    }
  }

  return(tidy_list)

}

# for when there are multiple parameters
tidy_samples_df <- function(out_list){

  tidy_list <- list()
  for(i in seq_along(out_list)){

    wei <- as.data.frame(unlist(out_list[[i]]$weights))
    colnames(wei) <- "log_weights"

    samples_data <- out_list[[i]]$samples$`__data__`

    param_lengths <- apply(samples_data, 2, FUN = function(x) length(x[[1]]))
    split_this <- names(which(param_lengths > 1))

    tidy_df <- samples_data %>%
      tidyr::unnest_wider(dplyr::all_of(split_this), names_sep = "_") %>%
      dplyr::mutate(sample = 1:nrow(.data), .before = 1) %>%
      dplyr::bind_cols(tibble::tibble(log_weights = dplyr::pull(wei),
                                      weights = exp(.data$log_weights - max(.data$log_weights)),
                                      norm_weight_within_sweep = .data$weights/sum(.data$weights))) %>%
      dplyr::mutate(normConst = out_list[[i]]$normConst) %>%
      as.data.frame()

    tidy_list[[i]] <- tidy_df
  }

  return(tidy_list)

}

# for when there is only one parameter
tidy_samples_vec <- function(out_list){

  tidy_list <- list()
  for(i in seq_along(out_list)) {
    tidy_df <- as.data.frame(out_list[[i]]) %>%
      dplyr::rename(log_weights = .data$weights, parameter = .data$samples) %>%
      dplyr::mutate(sample = 1:nrow(.data), .before = 1) %>%
      dplyr::mutate(weights = exp(.data$log_weights - max(.data$log_weights)),
                    norm_weight_within_sweep = .data$weights/sum(.data$weights),
                    .before = "normConst")
    tidy_list[[i]] <- tidy_df
  }

  return(tidy_list)

}


sub_sample_sweep <- function(out = NULL, sample_freq = NULL){

  nsamples <- floor(nrow(out)/sample_freq)
  if(nsamples == 0) nsamples <- 1

  subset <- out %>%
    dplyr::slice_max(.data$norm_weights, n = nsamples) %>%
    as.data.frame()

  return(subset)
}



