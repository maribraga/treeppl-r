#' Read output from inference with TreePPL
#'
#' @param filename Path to json file with the output from inference with TreePPL
#'
#' @return A data frame with one row for each sample and columns for parameters and weights for each run.
#' If the file contains the output from multiple runs, returns a list of data frames.
#' @export
#'
#' @examples
read_treeppl_output <- function(filename = NULL, sample_freq = NULL){

  # check if file has multiple lines
  nlines <- system2("wc", c("-l", filename), stdout = TRUE) %>%
    strex::str_first_number()

  if( nlines > 1 ){
    # amend json format
    system2(system.file("json_amend.zsh", package = "treepplr"), filename)

    # read file and then create a list of outputs
    out <- separate_outs(filename)  %>%
      lapply(tidy_samples)

    if(!is.null(sample_freq)){
      out <- lapply(out, FUN = function(x) sub_sample_sweep(x, sample_freq))
    }

  } else{
    # read json file and make it a data frame
    out <- tidy_samples(filename)

    if(!is.null(sample_freq)){
      out <- sub_sample_sweep(out, sample_freq)
    }
  }
  return(out)
}


separate_outs <- function(json_file = NULL){

  if(class(json_file) == "character"){
    out_json <- jsonlite::fromJSON(json_file)
  } else if(class(json_file) == "list") {
    out_json <- json_file
  }

  out_list <- list()

  for(i in seq_along(out_json$samples)) {
    out_list[[i]] <- list(samples = out_json$samples[[i]],
                          weights = out_json$weights[[i]],
                          normConst = out_json$normConst[i])
  }

  return(out_list)

}


tidy_samples <- function(json_file = NULL) {

  if(class(json_file) == "character"){
    out_list <- jsonlite::fromJSON(json_file)
  } else if(class(json_file) == "list") {
    out_list <- json_file
  }

  wei <- as.data.frame(unlist(out_list$weights))
  colnames(wei) <- "log_weights"
  wei <- wei[1:(nrow(wei)-1), ] %>% as.numeric()

  params <- as.data.frame(out_list$samples$`__data__`) %>%
    tidyr::unnest_wider(3, names_sep = ",") %>%
    dplyr::mutate(iter = 1:nrow(.), .before = 1)
  params <- params[1:(nrow(params)-1),]
  colnames(params) <- c("iteration", "clock","beta", "gain_01", "loss_10", "gain_12", "loss_21")

  out <- params %>%
    dplyr::bind_cols(tibble::tibble(log_weights = wei,
                     weights = exp(log_weights - max(log_weights)),
                     norm_weights = weights/sum(weights))) %>%
    dplyr::mutate(normConst = out_list$normConst[[1]]) %>%
    as.data.frame()

  return(out)

}


sub_sample_sweep <- function(out = NULL, sample_freq = NULL){

  nsamples <- floor(nrow(out)/sample_freq)

  subset <- out %>%
    dplyr::slice_head(norm_weights, n = nsamples) %>%
    as.data.frame()

  return(subset)
}
