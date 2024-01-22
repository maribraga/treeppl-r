#' Compile and run a TreePPL program
#'
#' @param source File path to TreePPL source program.
#' @param data_path Complete path to JSON file with input data. Created by `treeppl_input()`.
#' @param method Inference method to be used.
#' @param samples The number of samples/particles during inference. Default is 1000 samples.
#' @param chains The number of chains/sweeps. Each chain will contain the number of samples passed in `samples`. Default is 1 chain.
#' @param out_path Path to JSON file outputed by TreePPL.
#' @param name_exe Name of the executable compiled from `source`.
#' @param sample_freq For each sweep/chain, sample one particle/sample every `sample_freq` particles/samples.
#'
#' @return A data frame with sampled values, log weights, normalized weights, and the normalizing constant for all samples.
#' @export
#'
#' @examples
#' \dontrun{
#'   coinflips <- tibble(coinflips = sample(c(TRUE, FALSE), 20, replace = TRUE))
#'.  input <- treeppl_input(coinflips)
#'   output <- run_treeppl(source = paste0(system.file("extdata", package = "treepplr"),"/coin.tppl"),
#'                         data_path = input,
#'                         samples = 10)
#' }
run_treeppl <- function(source, data_path, method = "smc-bpf", samples = 1000, chains = 1, out_path = NULL, name_exe = "out", sample_freq = NULL) { # smc-apf

  dir <- sub("[^/]+.tppl","", source)

  # check inputs
  if(method == "smc-apf") samples <- samples + 1
  if(is.null(out_path)) out_path <- paste0(dir,"stdout.json")

  # Compile program
  system2(command = "tpplc", args = c(source,                                           # treeppl source code
                                      paste0("-m ", method),                            # inference method
                                      paste0("--output ", dir, "/", name_exe)))         # name of the outputed executable
  # which arguments are necessary other than method?
  # should the executable go to a temporary folder and be delete afterwards?

  # run
  system2(command = paste0(dir,"/",name_exe),
          args = c(data_path,
                   paste0(samples," ",chains)),
          stdout = out_path
  )

  # read output
  output <- read_treeppl_output(out_path, sample_freq)

  return(output)
}

