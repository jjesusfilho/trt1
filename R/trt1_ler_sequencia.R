#' Lê metadados das decisões do TRT1 baixados com trt1_baixar_sequencia
#'
#' @param arquivos Vetor de arquivos
#' @param diretorio Informar diretório se não informar arquivos
#'
#' @return tibble
#' @export
#'
trt1_ler_sequencia <- function(arquivos = NULL, diretorio = "."){

  if (is.null(arquivos)){

    arquivos <- list.files(diretorio, full.names = TRUE)

  }

  pb <- progress::progress_bar$new(total = length(arquivos))


  purrr::map_dfr(arquivos,purrr::possibly(~{

  pb$tick()

    x <- xml2::read_html(.x)


          x %>%
          xml2::xml_find_first("//table[@class='panel-body table itemDisplayTable']") %>%
          rvest::html_table() %>%
          dplyr::select(var = 1, val = 2) %>%
          tidyr::pivot_wider(names_from = var, values_from = val) %>%
          janitor::clean_names() %>%
          dplyr::mutate(url_pdf = x %>%
                          xml2::xml_find_first("//table[@class='table panel-body']//a") %>%
                          xml2::xml_attr("href") %>%
                          paste0("https://bibliotecadigital.trt1.jus.br/",.)
          ) %>%
         dplyr::mutate(dplyr::across(dplyr::starts_with("data"), ~stringr::str_extract(.x,".{10}") %>%
                                                               lubridate::ymd())) %>%
         dplyr::mutate(sequencia = stringr::str_extract(.x,"\\d+(?=\\.html)"),.before =1)


  },NULL))


}
