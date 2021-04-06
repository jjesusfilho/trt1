#' Baixa julgados de primeiro e segundo grau do TRT1 pela sequência
#'
#' @param x Número da sequência
#' @param diretorio Diretório
#'
#' @return  html
#' @export
#'
trt1_baixar_sequencia <- function(x, diretorio = ".") {

   url <- "https://bibliotecadigital.trt1.jus.br/jspui/handle/1001/"


  pb <- progress::progress_bar$new(total = length(x))

  purrr::walk(x, purrr::possibly(~ {
    pb$tick()

    url1 <- paste0(url, .x, "?mode=full")

    arquivo <- file.path(diretorio, paste0(stringr::str_replace_all(Sys.time(), "\\D", "_"), "_sequencia_", .x, ".html"))

    httr::GET(url1, httr::write_disk(arquivo, overwrite = TRUE))

  }, NULL))
}
