#' Selecciona categorias para classificar los datos
#'
#' Està funcion analiza los datos a traves de Gemini, i genera una lista, un vector con las categorias que considere oportuna, i un elemento de texto donde encontramos una descripcion de cada categoria, para entender mejor que abarca.
#'
#' @param dt Character vector.
#' @param nmax_cat numeric
#' @param sol character
#' @param api_key character
#' @param cont character
#' @param temperature numeric
#' @return list un vector de caracteres y un elementto caracte
#' datos <- system.file(
#' "extdata",
#' "enquesta_satisfaccio.txt",
#' package = "IAIAdata"
#' )
#' api <- system.file(
#' "extdata",
#' "api_key.txt",
#' package = "IAIAdata"
#' )
#' @examples
#' class <- select_cat(dt = dades,
#'                     api_key = api,
#'                     sol = "Explicar las inquietudes de los pacientes")
#' @import dplyr
#' @importFrom stringr str_extract str_split str_trim str_replace_all str_locate_all str_sub str_remove_all
#' @importFrom R.utils withTimeout
#' @importFrom stats complete.cases
#' @importFrom ellmer chat_gemini
#' @export
select_cat <- function(dt,
                       nmax_cat="",
                       sol = "",
                       api_key,
                       cont = "",
                       temperature = 0.5) {



  if (!is.numeric(temperature)){
    stop("Error: El parámetro debe ser un valor numérico entre 0 y 1")
  }

  if (is.null(api_key)| !is.character(api_key)) {
    stop("Error: Necesitas un api key de google para poder usar esta funcion. Esta debe intoducirse como elemento character")
  }

  if (!is.character(sol)) {
    stop("Error: El parametro sol ha ser ser un elemento de texto que explique como han de ser las categorias.")
  }

  if (!is.numeric(nmax_cat) & nmax_cat != as.integer(nmax_cat) & nmax_cat!=""){
    stop("Error: nmax_tematicas ha de ser un numero entero que defini al numero de categorias que se quiere obtener")
  }

  sys_promp <- "Ten un tono formal pero sin usar tecnicismos, tu funcion es entregar: \n -Primero una lista solo con los nombres de las categorias para classificar los datos que se te entreguen. \n -Luego muestra otra vez cada ecategoria junto a una explicacion de que abarca o contiene cada una de ellas. \n (Nunca clasifiacar las respuestas).  Cuando des una categoria pon delante una C y un codigo numerico de 2 numeros, indicando el numero de la categoria. \n Es indispensable que esten estas tres categorias -C99 NA- con valores vacios o NA, -C98 Respuestas fuera de contexto- respuestas sin sentido para el contexto, -C97 Respuestas vacias- repuestas que no estan dando informacion aun que si respondondan a la pregunta, estas categorias no se cuentan si se solicita un numero de categotias determinado."

  dt_uniques <- unique(dt)
  if(sol == ""){
    prompt_fun <- paste("Elavora una lista de ", nmax_cat," categorias para classificar las siguientes respuestas:\n", paste0(dt_uniques, collapse = "\n"),cont, "Entrega primero una lista solo con los identificadores y los nombres de las categorias y luego cada identificador y categoria con su descripcion. ")
  } else {
      prompt_fun <-paste( "De las respuestas siguientes respuestas:\n", paste0(dt, collapse = "\n"), "\n Haz una lista de", nmax_cat,"categorias que puedan classificar esas respuestas para", sol, cont," Entrega primero una lista solo con los identificadores y los nombres de las categorias y luego cada identificador y categoria con su descripcion.  y una extra para NA o respuestas sin información")
    }

  # El parametre temperature controla la aleatoritat, pren valors entre 0 i 1, definim temperature baixa per si tornem a executar tindre resultats més similars
  resposta_gemini <- chat_gemini(system_prompt = sys_promp, api_key = api_key, api_args =list(list(temperature = temperature))) %>% invisible()
  cat_generadas <- resposta_gemini$chat(prompt_fun)


  eliminar_signes <- function(texte) { # valorar extreure de la funcio
    gsub("[^a-zA-ZáéíóúÁÉÍÓÚüÜñÑ0-9\n]", " ", texte)
  }

  cat_0 <- cat_generadas %>% eliminar_signes()

  lineas <- str_split(cat_generadas, "\n")[[1]] %>%
    str_trim() %>%
    keep(~ str_detect(.x, "\\bC\\d{2}\\b")) %>%
    eliminar_signes()


  # Seleccionar només les cat sense explicacions, guardem explicacions a part
  codigos <- str_extract(lineas, "C\\d{2}") # identificador per tindre ubicades les lineas amb categories

  # 2. Encontrar la posición de la primera repetición
  primera_rep <- which(duplicated(codigos))[1]

  # 3. Tomar sólo hasta justo antes de esa repetición
  if (!is.na(primera_rep)) {
    lineas_limpias <- lineas[1:(primera_rep - 1)]
  } else {
    lineas_limpias <- lineas
  }

  cat <- str_replace_all(lineas_limpias, "C\\d{2}.", "") %>%
    sub("\\s+$", "", .) %>%
    sub("^\\s+", "", .)

  # 1. Localizar todas las posiciones de inicio de "C01"
  posiciones <- str_locate_all(cat_generadas, "C01")[[1]][, "start"]

  # 2. Tomar la que corresponde a la segunda aparición
  des_0 <- posiciones [2]

  # 3. Extraer desde ahí hasta el final

  #####################################################3
  explicacio <- str_sub(cat_generadas, des_0) %>%
    str_remove_all("\\*") %>%
    str_trim() %>%
    gsub("C\\d{2}", "", .)
 #########################################################

  return(list(cat = cat, desc = explicacio))
}

