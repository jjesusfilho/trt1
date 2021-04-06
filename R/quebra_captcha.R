
quebra_captcha <- function(file){

  x <- magick::image_read(file)

  x <-   x %>%
    image_chop(,geometry_area(0,30,0,+3)) %>%
    image_rotate(180) %>%
    image_chop(geometry_area(0,20,0,3)) %>%
    image_rotate(180) %>%
    image_median(1.2) %>%
    image_median(2.2) %>%
    image_median(3.2) %>%
    image_median(3.5)


  chars <- "abcdefghijklmnopqrstuvwxyz0123456789"
  n <- tesseract(options = list(tessedit_char_whitelist = numbers))

  chars <- tesseract(options = list(tessedit_char_whitelist = chars))

  image_write(im2,density="300x300") %>%
    ocr(engine = chars)
  x <- stringr::str_trim(x)


  if (nchar(x) == 7){

    return(x)

  }

}


