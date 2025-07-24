#' Selecciona categorias para classificar los datos
#'
#' Està funcion analiza los datos a traves de Gemini, i genera una lista, un vector con las categorias que considere oportuna, i un elemento de texto donde encontramos una descripcion de cada categoria, para entender mejor que abarca.
#'
#' @param dt vector de elementos caracter
#' @param cat lista o vector con las categorias para classificar los datos, en el caso de lista ha de tener un element desc con la descripccion de las categorias y un elemento cat, vector con las categorias
#' @param ncat numeric
#' @param sol_tem elemento character para pedir a la funcion para que van a ser las categorias, solo si no se incluyen categorias
#' @param api_key elemento caracter con la google api para conectar la funcion con gemini
#' @param temperature numeric
#' @param cat_temperature numeric
#' @param cont elemento character para contextualizar los datos
#' @return list un vector de caracteres con la classificacion, un elemento lsita con las categorias, y los datos originales
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
#'
#' datos <- readLines(enq)
#' api <- readLines(api_k)
#' class <- classifai(dt = datos,
#'                    api_key = api,
#'                    sol_tem = "Explicar las inquietudes de los pacientes")
#' @import dplyr
#' @importFrom stringr str_extract str_split str_trim str_replace_all str_locate_all str_sub str_remove_all
#' @importFrom R.utils withTimeout
#' @importFrom stats complete.cases
#' @importFrom ellmer chat_gemini
#' @export


classifai <- function(
    dt,
    cat = NULL,
    ncat = "",  #limitar
    sol_tem = NULL,
    api_key =  NULL,
    temperature=0.5,
    cont = "",
    cat_temperature=0.5)  {

  dt = dt %>% as.character()
  #avisos d'error de format
  if (!is.numeric(temperature) | temperature <= 0){
    stop("Error: El parámetro temperature debe ser un valor numérico entre 0 y 1")
  }


  if (is.null(api_key)| !is.character(api_key)) {
    stop("Error: Necesitas un api key de google para poder usar esta funcion. I esta debe intoducirse como elemento character")
  }

  if ((!is.numeric(ncat) | ncat != as.integer(ncat))&  ncat!= "" ){
    stop("Error: ncat ha de ser un numero entero que defini al numero de categorias que se quiere obtener")
  }

  if (!is.list(cat) & !is.vector(cat) & !is.null(cat)){
    stop("Error: Tematicas ha de ser una lista con un elemento cat con las categorias y un elemento des con las descriociones \n O un vector con las categorias. \n Puedes no dar este parametro y la funcion te dara unas categorias.")
  }

  respuestas_unicas <- unique(dt)

  if(is.null(cat)){
    if(is.null(sol_tem)){cat <- select_cat(dt=dt,api_key = api_key, nmax_cat = ncat, temperature = cat_temperature)} else {
      cat <- select_cat(dt = dt, api_key = api_key, sol = sol_tem, nmax_cat = ncat, temperature = cat_temperature)
    }
  }

  if(inherits(cat, "list")){
    promp_cat <- paste("Las categorias son:", paste0(cat[[1]], collapse = ", "), "\n Con estas descripciones de cada una:", cat[[2]])
    }  else {promp_cat <- paste("Las categorias son:", paste0(cat, collapse = ", ")) }

  sys_promp <- "Responde como una maquina, par cada entrada que recibas (Identificador de 8 caracteres y su elemnteo), da una unica respuesta de la permitidas"
  tokens <- stringi::stri_rand_strings(length(respuestas_unicas), 8, pattern = "[A-Z0-9]")

  dt_input<-tibble(id=tokens, valor=respuestas_unicas)
  # dt=dades$rb_sat_p7
  dt_text <- paste(paste(tokens, respuestas_unicas), collapse = "\n")
  promp <- paste0(
    "Clasifica las siguientes respuestas", dont, " en la categoria que mas se adecue ",
    promp_cat,
    ".\n I las respuestas a clasificar són: \n",dt_text, "\n")

  resposta_gemini <- chat_gemini(system_promp = sys_promp,
                                 api_key = api_key,
                                 api_args =list(list(temperature = temperature)))
  clasificacions <- resposta_gemini$chat(promp)

  #creem vector de dt
  classificacions_vector <- unlist(strsplit(clasificacions, "\n"))# %>%

  #eliminem caractes generets per format de resposta gemini
  classificacions_vector <- respuestas_limpias <- gsub("[^A-Za-z0-9 ÁÉÍÓÚáéíóúÑñÜüüçÇ()]", "", classificacions_vector)

  #eliminem si hi ha espais previs al id
  classificacions_vector <- respuestas <- sub("^[^A-Za-z0-9]+", "", classificacions_vector)
  tokens_output <- substring(classificacions_vector, 1, 8) # tokens_output <- c(tokens_output, "AAA111")
  classificacio <- substring(classificacions_vector, 10)  # classificacio <- c(classificacio, "nou nivell")
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

    return(list(input = classificacio_final$valor,
                categories = cat,
                classification = classificacio_final$classificacio))
}
