library(tidyverse)
library(RSelenium)


driver <- RSelenium::rsDriver(browser = "chrome",
                              port=45689L,
                              chromever = 
                                system2(command = "wmic",
                                                  args = 'datafile where name="C:\\\\Program Files\\\\Google\\\\Chrome\\\\Application\\\\chrome.exe" get Version /value',
                                                  stdout = TRUE,
                                                  stderr = TRUE) %>%
                                stringr::str_extract(pattern = "(?<=Version=)\\d+\\.\\d+\\.\\d+\\.") %>%
                                magrittr::extract(!is.na(.)) %>%
                                stringr::str_replace_all(pattern = "\\.",
                                                         replacement = "\\\\.") %>%
                                paste0("^",  .) %>%
                                stringr::str_subset(string =
                                                      binman::list_versions(appname = "chromedriver") %>%
                                                      dplyr::last()) %>%
                                as.numeric_version() %>%
                                max() %>%
                                as.character())
remote_driver <- driver[["client"]]

#====== FUNÇÕES AUXILIARES ========
findElement_ = function(path, click=T){
  find = remote_driver$findElement(using = 'xpath', value = path)
  
  if(click==T) { 
    remote_driver$executeScript('arguments[0].scrollIntoView({block: "center", behavior: "smooth"});', list(find))
    find$clickElement()
  } else {}
  
  return(find)
}
tryElement_ = function(path, FUN = NULL, try = 10, click=T){
  x = NULL
  i = 1
  
  try(
  while (is.null(x) & i<= try) {
  
  i = i+1
  tryCatch({x = findElement_(path, click)
            return(x)},
           error = if(is.null(FUN)) function(e) Sys.sleep(1) else FUN)
    }
  )
  
}
tryElements_ = function(path, FUN = NULL, try = 10){
  x = NULL
  i = 1
    try(
    while (is.null(x) & i<= try) {
      i = i+1
      tryCatch({x = remote_driver$findElements(using = 'xpath', value = path)
                return(x)},
      error = if(is.null(FUN)) function(e) Sys.sleep(1) else FUN)
    
    }
    )
}
rm_accent <- function(str,pattern="all") {
  if(!is.character(str))
    str <- as.character(str)
  
  pattern <- unique(pattern)
  
  if(any(pattern=="Ç"))
    pattern[pattern=="Ç"] <- "ç"
  
  symbols <- c(
    acute = "áéíóúÁÉÍÓÚýÝ",
    grave = "àèìòùÀÈÌÒÙ",
    circunflex = "âêîôûÂÊÎÔÛ",
    tilde = "ãõÃÕñÑ",
    umlaut = "äëïöüÄËÏÖÜÿ",
    cedil = "çÇ"
  )
  
  nudeSymbols <- c(
    acute = "aeiouAEIOUyY",
    grave = "aeiouAEIOU",
    circunflex = "aeiouAEIOU",
    tilde = "aoAOnN",
    umlaut = "aeiouAEIOUy",
    cedil = "cC"
  )
  
  accentTypes <- c("´","`","^","~","¨","ç")
  
  if(any(c("all","al","a","todos","t","to","tod","todo")%in%pattern)) # opcao retirar todos
    return(chartr(paste(symbols, collapse=""), paste(nudeSymbols, collapse=""), str))
  
  for(i in which(accentTypes%in%pattern))
    str <- chartr(symbols[i],nudeSymbols[i], str) 
  
  return(str)
}

#====== COLETA DOS LINKS ========
munics = tibble(
  
  Angra_dos_Reis = "https://www.tripadvisor.com.br/Hotels-g303489-Angra_Dos_Reis_State_of_Rio_de_Janeiro-Hotels.html",
  Cabo_Frio = "https://www.tripadvisor.com.br/Hotels-g311322-Cabo_Frio_State_of_Rio_de_Janeiro-Hotels.html"
  
) %>% pivot_longer(cols = everything(),names_to = "nome", values_to = "link") 


map(1:length(munics), 

function(i){
remote_driver$deleteAllCookies()
remote_driver$navigate(munics$link[i])
tryElement_('//*[@id="onetrust-accept-btn-handler"]') #aceitar cookies

#Selecionar datas:
findElement_('//*[@id="PERSISTENT_TRIP_SEARCH_BAR"]//*[@data-automation="checkin"]') #Abre caixa de seleção da data


Sys.setlocale("LC_TIME", "English")
date_in = str_to_sentence(format.Date("2023-06-22", "%b %d %Y")) #data check-in
date_out = str_to_sentence(format.Date("2023-06-30", "%b %d %Y")) #data check-out


tryElement_(paste0("//*[contains(@aria-label, '", date_in, "')]"),
            function(e) {findElement_('//*[@data-testid="nav_next"]')}, 9999)
tryElement_(paste0("//*[contains(@aria-label, '", date_out, "')]"))
tryElement_("//*[@data-automation='guestsUpdateBtn']")


n_max = tryElements_('//*[@data-component-init="data-component-init"]', 25)
n_max = n_max %>% 
        map(~.$getElementText()) %>% unlist() %>% 
        .[matches("estabelecimento", vars = .)] %>% 
        parse_number()
print(n_max)

get_links = map(
  1:ceiling(n_max/30),
  function(pages) {
  
    next_ = function (e) tryElement_('//*[@class="nav next ui_button primary"]', 100) #próxima página
    prev_ = function (e) tryElement_('//*[@class="nav previous ui_button secondary"]', 100) #página anterior
    loading_ = function (e) {tryElement_('//*[@data-placement-name="hotels_loading_box"]', 100)$getElementAttribute("style") %>% 
                             unlist() %>% 
                             grepl("block", x = .)}
    
  links = tryElements_("//*[@data-clicksource='HotelName']",
                       function(e) {return(NA)}) %>% 
          map_chr(~.$getElementAttribute("href") %>% as.character)
  
  if(pages<ceiling(n_max/30)){
    next_()
    while(loading_()==T) Sys.sleep(1)
  } else {}

  
  return(links)
  }
)
all_links = unique(unlist(get_links))

saveRDS(all_links, paste0("all_links_",munics$nome[i],".rds"))
return(all_links)
Sys.sleep(5)
}
)

#====== COLETA DAS INFOS ========

teste = map_df(sample(all_links_Angra_dos_Reis, 1), function(i){
remote_driver$deleteAllCookies()
remote_driver$navigate(i)
tryElement_('//*[@id="onetrust-accept-btn-handler"]') #aceitar cookies

print(i)

# Nome e Endereço
nome = tryElement_("//*[@id='component_3']", click = F)$getElementText() %>% unlist() %>% 
       str_split("\n") %>% unlist() %>% 
        .[!grepl("avaliações|avaliação", .)] %>% 
        .[!grepl("Avaliação", .)] %>% 
        .[!grepl("Salvar", .)] %>% 
        .[!grepl("Compartilhar", .)] %>%
        .[!grepl("Este estabelecimento é seu?", .)] %>%
        .[!grepl("Visitar o site do hotel", .)] %>%
        .[!grepl("Enviar e-mail para o hotel", .)] %>%
        .[!grepl("WhatsApp", .)] %>%
        .[!grepl("Nº [[:digit:]]+ de [[:digit:]]+", .)] %>%
        .[!grepl("\\([[:digit:]]{2}\\)", .)] %>% 
        .[!grepl("[[:digit:]]+º de [[:digit:]]+", .)] %>% 
        .[!grepl("#[[:digit:]]+ de [[:digit:]]+", .)] %>% 
        matrix(., nrow = 1, byrow = T) %>% as.tibble() %>% 
        rename(Nome = 1, Endereco = 2) %>% select(1:2)

# Avaliações
aval_geral = tryElements_('//*[@id="ABOUT_TAB"]/div[2]/div[1]/div[1]') %>% 
             map(~.$getElementText())%>% unlist() %>% 
             str_split("\n") %>% unlist() %>% 
             .[!grepl("Você já esteve em", .)] %>% 
             .[!grepl("ainda não recebeu avaliações", .)] %>% 
             .[!grepl("Faça uma avaliação", .)] %>% 
             .[!grepl("Aprimorar este perfil", .)] %>% 
             .[!grepl("Recomende alterações para melhorar nosso conteúdo", .)] %>% 
              matrix(., nrow = 1, byrow = T) %>% as.tibble()

if(length(aval_geral) == 3){
  aval_geral = aval_geral %>% rename(Nota = 1, Nota_desc = 2, Avaliacoes = 3)
}  else aval_geral = NULL



if(length(tryElements_('//*[@id="ABOUT_TAB"]/div[2]/div[1]/div'))<=6 ){
aval=NULL
} else {
  
aval1 = tryElements_('//*[@id="ABOUT_TAB"]/div[2]/div[1]/div') %>% 
        map(~.$getElementText()) %>% unlist()
which_ = min(which(aval1=="")-1)
aval1 = aval1[2:which_]

aval2 = tryElements_('//*[@id="ABOUT_TAB"]/div[2]/div[1]/div') %>%
       .[2:which_] %>% 
       map(~.$findChildElement(using ="xpath", "span")$getElementAttribute("class")) %>% unlist() %>% 
       parse_number() %>% 
       map_dbl(., ~./10)

aval = paste(aval1, aval2, sep = ": ") %>% 
       matrix(., nrow = 1, byrow = T) %>% as.tibble() %>% 
       set_names(.[1,]) %>% 
       rename_all(~gsub(":.*","",.))
}

# Descrição do hotel
descr = tryElements_('//*[@id="ABOUT_TAB"]//*[@class="fIrGe _T"]')%>% 
        map(~.$getElementText())%>% unlist() %>% 
        if (length(.) > 0) matrix(., nrow = 1, byrow = T) %>% as.tibble() %>% rename(Descricao = 1) else .
  

# Comodidades
html = remote_driver$getPageSource() %>% unlist() %>% 
       rvest::read_html()

atrib_nome = html %>% 
             rvest::html_elements(xpath = '//*[@class="aeQAp S5 b Pf ME"]') %>% 
             rvest::html_text2() %>% 
             .[!grepl("Detalhes úteis", .)] %>% 
              if(length(.)>3) .[1:3] else .

atrib =  html %>% 
         rvest::html_elements(xpath = '//*[@class="OsCbb K"]') %>% 
         rvest::html_text2() %>% 
         map(~str_split(., "\n") %>% unlist()) %>% 
         set_names(atrib_nome) %>% 
         imap(~paste0(.y %>% 
                      gsub("Serviços do estabelecimento", "Estab_",.) %>% 
                      gsub("Comodidades nos quartos", "Quarto_",.) %>% 
                      gsub("Tipos de quarto", "TipoQrt_",.), .x)) %>% unlist() %>% 
          if (length(.) > 0) matrix(., nrow = 1, byrow = T) %>% as.tibble() %>% set_names(.[1,]) %>% mutate_all(~1)  else .

# Lat-long
loc = tryElement_('//*[@data-test-target="staticMapSnapshot"]/img')

loc = if(!is.null(loc)){ loc$getElementAttribute("src") %>% 
                   unlist() %>% str_split("&") %>% unlist() %>% 
                   .[grepl("center=", .)] %>% 
                   gsub("center=", "", .) %>% 
                   str_split(",") %>% unlist() %>% 
                   matrix(., nrow = 1, byrow = T) %>% as.tibble() %>% rename(Lat = 1, Long=2)} else NULL

# Rodapé
rodape_nome = html %>% 
              rvest::html_elements(xpath = '//*[@class="mpDVe Ci b"]') %>%
              rvest::html_text2()

rodape = html %>% 
         rvest::html_elements(xpath = '//*[@class="IhqAp Ci" or @class="IhqAp Ci Wg"]') %>% as.character() %>% 
         gsub("<[^>]+>","\n", .) %>% 
         str_split("\n") %>%
         map(~.[.!=""] ) %>% 
         set_names(rodape_nome) %>% 
         map(~paste(., collapse = ", ") ) %>% as.tibble() %>% 
         rename_all(~str_to_sentence(rm_accent(.)))

link = tibble(Link = i)

result = bind_cols(nome, loc, rodape, descr, aval_geral, aval, atrib, link)

  
return(result)
})

#====== COLETA DOS PREÇOS ========

preco = map_df(sample(all_links_Angra_dos_Reis, 1), function(i){
  
  remote_driver$deleteAllCookies()
  remote_driver$navigate(i)
  tryElement_('//*[@id="onetrust-accept-btn-handler"]') #aceitar cookies
  
  print(i)
  
  tryElement_('//*[@data-test-target="picker-CHECKIN"]') #Abre caixa de seleção da data
  
  
  Sys.setlocale("LC_TIME", "English")
  date_in = str_to_sentence(format.Date("2023-01-22", "%b %d %Y")) #data check-in
  date_out = str_to_sentence(format.Date("2023-01-30", "%b %d %Y")) #data check-out
  
  
  tryElement_(paste0("//*[contains(@aria-label, '", date_in, "')]"),
              function(e) {findElement_('//*[@data-testid="nav_next"]')}, 9999)
  tryElement_(paste0("//*[contains(@aria-label, '", date_out, "')]"))

  # Preço
  ofertas = tryElements_("//*[@class='premium_offers_area offers']//*[text()='Ver oferta']")
  
  
  # Nome
  nome = tryElement_("//*[@id='component_3']", click = F)$getElementText() %>% unlist() %>% 
    str_split("\n") %>% unlist() %>% 
    .[!grepl("avaliações|avaliação", .)] %>% 
    .[!grepl("Avaliação", .)] %>% 
    .[!grepl("Salvar", .)] %>% 
    .[!grepl("Compartilhar", .)] %>%
    .[!grepl("Este estabelecimento é seu?", .)] %>%
    .[!grepl("Visitar o site do hotel", .)] %>%
    .[!grepl("Enviar e-mail para o hotel", .)] %>%
    .[!grepl("WhatsApp", .)] %>%
    .[!grepl("Nº [[:digit:]]+ de [[:digit:]]+", .)] %>%
    .[!grepl("\\([[:digit:]]{2}\\)", .)] %>% 
    .[!grepl("[[:digit:]]+º de [[:digit:]]+", .)] %>% 
    .[!grepl("#[[:digit:]]+ de [[:digit:]]+", .)] %>% 
    matrix(., nrow = 1, byrow = T) %>% as.tibble() %>% 
    rename(Nome = 1, Endereco = 2) %>% select(1)
  
  link = tibble(Link = i)
  
  
  
  result = bind_cols(nome, link)
  
  
  return(result)
})
















