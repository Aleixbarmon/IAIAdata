#' Selecciona categorias para classificar los datos
#'
#' Està funcion analiza los datos a traves de Gemini, i genera una lista, un vector con las categorias que considere oportuna, i un elemento de texto donde encontramos una descripcion de cada categoria, para entender mejor que abarca.
#'
#' @param dt Character vector.
#' @param api_key character
#' @param cont character
#' @param temperature numeric
#' @return data.frame con una columna por categoria indicando para cada elemento de la entrada si petenece o no a esa categoria
#' @examples
#' enq <- system.file(
#' "extdata",
#' "enquesta_satisfaccio.txt",
#' package = "IAIAdata"
#' )
#' api_k <- system.file(
#' "extdata",
#' "api_key.txt",
#' package = "IAIAdata"
#' )
#' datos <- readLines(enq)
#' api <- readLines(api_k)
#' classificacio_binaria_resultat <- classifai_bin(dt = datos, cat= cat, api_key = api)
#' @import dplyr
#' @importFrom stringr str_extract str_split str_trim str_replace_all str_locate_all str_sub str_remove_all
#' @importFrom R.utils withTimeout
#' @importFrom stats complete.cases
#' @importFrom ellmer chat_gemini
#' @export
classifai_bin <- function(dt,
                          cat,
                          api_key,
                          temperature=0.05){

  dt[is.na(dt)] <- "NA"
  respuestas_unicas <- unique(dt)
  tokens <- stringi::stri_rand_strings(length(respuestas_unicas), 8, pattern = "[A-Z0-9]")

  dt_input<-tibble(id=tokens, valor=respuestas_unicas)
  dt_text <- paste(paste(tokens, respuestas_unicas), collapse = "\n")

  classificacio <- cbind()
  if(inherits(cat, "list")){
    n <-length(cat[[1]])
    categories <- cat[[1]]
    desc  <- c()
    prompt_fun <- c()

    for(i in 1:n){
      if(i<n){
        patro <- paste0(categories[i],"(.|\\n)*?(?=",categories[i+1],":)")
        desc[i] <- str_extract(cat[[2]], patro)
        prompt_fun[i] <- paste0("Dime si los textos de acontunuación tratan:  ", categories[i],"\n",
        desc[i], "\n Los textos son:\n ", dt_text)

      } else {
        patro2 <- paste0(categories[i],"(.|\\n)*")
        desc[i] <- str_extract(cat[[2]], patro2)
        prompt_fun[i] <- paste0("Dime si los textos de acontunuación tratan:  ", categories[i],"\n",
                                desc[i], "\n Los textos son:\n ", dt_text)
        }
    }
  } else {
    n <- length(cat)
    categories <- cat
    desc = ""
    for(i in 1:n){
      prompt_fun[i] <- paste0("Dime si los textos de acontunuación se podrian classificar en esta categoria", categories[i],"\n Los textos son:\n ", dt_text)
      prompt_fun[i] <- sub("\n[^\n]*$", "",  prompt_fun[i])
    }
  }

  sys_promp = "Eres una maquina solo da 1 en caso afirmativo, 0 en negativo para cada entrada (Identificador de 8 caracteres y su elemnteo). Da para cada elemento su identificador seguido 0 o 1 segun corresponda (ej. AAAA1234 0)"
  resposta_gemini <- chat_gemini(system_prompt = sys_promp, api_key=api_key,
                                 api_args =list(list(temperature = temperature)))
  consulta <- function(prompt_fun){
    # si treiem el chat_gemini() del bucle les consultes tenen memoria

    repeat {
      classificacio <- tryCatch({

      clasificacions <- resposta_gemini$chat(prompt_fun)

      classificacions_vector <- unlist(strsplit(clasificacions, "\n"))# %>%
      #eliminem caractes generets per format de resposta gemini
      classificacions_vector <- respuestas_limpias <- gsub("[^A-Za-z0-9 ÁÉÍÓÚáéíóúÑñÜüüçÇ]", "", classificacions_vector)

      #eliminem si hi ha espais previs al id
      classificacions_vector <- trimws(classificacions_vector, which = "left")

      tokens_output <- substring(classificacions_vector, 1, 8) # tokens_output <- c(tokens_output, "AAA111")
      classificacio <- substring(classificacions_vector, 10)
      classificacio <- sub("^\\s+", "", classificacio)


      dt_output<- tibble(id = tokens_output, classificacio = classificacio)

      # eliminem files que no son dt
      id_regex <- "^[A-Za-z0-9]{8}$"
      first_id_row <- min(which(grepl(id_regex, dt_output$id)))

      if(first_id_row>1){
        dt_output <- dt_output[-c(1:first_id_row-1),]
      }


      resultat <- dt_input %>%
        left_join(dt_output, by = "id")

      # cbind(dt_text, dt_output)
      classificacio_final <- tibble(valor = as.character(dt)) %>%
        left_join(resultat, by = "valor")

      no_classificatas <- resultat %>%       # si hi ha dt no clasificades classificacio sera NA i
        filter(is.na(classificacio)) %>%         # valors classificats que no existeixin no apareixen
        pull(id)



      # return(c(classificacio_final$classificacio))}
      return(classificacio_final$classificacio)
      })

      # Si tryCatch NO devolvió NULL, significa que hubo éxito y ya hizo return()
      # En caso contrario, el bucle repeat vuelve a comenzar
    }
}
  for(i in 1:n){

    classificacio <- cbind(classificacio,consulta(prompt_fun[i]))
  }
  colnames(classificacio) <- categories

  #colnames(classificacio) <- cat[[1]][cat[[1]]!="NA"]
  for(col in names(classificacio)) {
    classificacio[[col]] <- factor(
      classificacio[[col]],
      levels = c(0, 1),
      labels = c("No", "Sí")
    )
  }

  return(cbind(dt,classificacio))
}

