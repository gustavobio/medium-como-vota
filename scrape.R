## Código da postagem https://medium.com/@ghcarvalho/presidenci%C3%A1veis-no-twitter-5fa2a7ec144b

require(rvest)
require(stringr)
require(dplyr)
require(purrr)
require(tidytext)
require(ggplot2)
require(wordcloud)
require(tm)
require(topicmodels)
require(igraph)
require(ggraph)

url <- "http://www.camara.leg.br/internet/sitaqweb/resultadoPesquisaDiscursos.asp?CurrentPage=1&txIndexacao=&BasePesq=plenario&txOrador=&txPartido=&dtInicio=25/10/2017&dtFim=25/10/2017&txUF=&txSessao=&listaTipoSessao=&listaTipoInterv=&inFalaPres=&listaTipoFala=&listaFaseSessao=&txAparteante=&listaEtapa=&CampoOrdenacao=dtSessao&TipoOrdenacao=DESC&PageSize=1000&txTexto=&txSumario="

links <- read_html(url) %>%
  html_nodes("td:nth-child(4)") %>%
  html_nodes("a") %>%
  html_attr("href") %>%
  str_replace_all("\\r\\n", "") %>%
  str_replace_all("\\s*(?=&)", "") %>%
  map(function(x) URLencode(paste("http://www.camara.leg.br/internet/sitaqweb/", x, sep = ""))) 
  
deputados <- read_html(url) %>%
  html_nodes("td:nth-child(6)") %>%
  html_text() %>%
  str_replace_all("\\r\\n\\s*", "")

fase <- read_html(url) %>%
  html_nodes("td:nth-child(3)") %>%
  html_text() %>%
  str_trim("both")

get_disc <- function(link) {
  read_html(link) %>%
    html_nodes("font") %>%
    html_text() %>%
    str_replace_all("\\t", "")
}

discursos <- map(links, get_disc) %>%
  lapply(paste, collapse = " ")

target <- unlist(lapply(discursos, function(x) grepl("Total\\s*(\\:){0,1}", x)))

discursos <- tibble(deputado = deputados[target], discurso = unlist(discursos[target]))

discursos <- discursos %>%
  mutate(deputado = unlist(lapply(strsplit(discursos$deputado, ", "), `[`, 1))) %>%
  mutate(partido = unlist(lapply(strsplit(discursos$deputado, ", "), `[`, 2))) %>%
  mutate(estado = gsub(".+-", "", partido)) %>%
  mutate(partido = gsub("-.+", "", partido)) %>%
  mutate(discurso = gsub("\\\"|\\\'", "", discurso)) %>%
  mutate(voto = str_extract(discurso, "\\w+(?=\\.{0,1} Total)")) %>%
  mutate(ordem = str_extract(discurso, ".(?<=[T|t]otal).*([0-9]+)")) %>%
  mutate(ordem = as.numeric(str_extract(ordem, "[0-9]+")))

discursos <- discursos %>%
  mutate(discurso_contagem = str_extract(discurso, "[OA]\\sSR[A]*.(.*?)(?=[OA]\\sSR)")) %>%
  mutate(discurso_contagem = str_replace(discurso_contagem, "[^-]*-.*?[-]", "")) %>%
  mutate(discurso_contagem = str_replace(discurso_contagem, "\\([^\\)]*\\)", "")) %>%
  mutate(discurso_contagem = str_replace(discurso_contagem, "\\.{2,}", "")) %>%
  mutate(discurso_contagem = trimws(discurso_contagem))

discursos <- discursos %>%
  mutate(discurso_limpo = gsub("Sr(\\.){0,1} Presidente", "", discurso_contagem)) %>%
  mutate(discurso_limpo = gsub("[S|s]im|[N|n]ão", "", discurso_limpo)) %>%
  mutate(discurso_limpo = gsub("[V|v]ot[o|a](s|r){0,1}", "", discurso_limpo)) %>%
  mutate(discurso_limpo = gsub("Bonifácio de Andrada", "", discurso_limpo)) %>%
  mutate(discurso_limpo = gsub("[R|r]elatório", "", discurso_limpo)) %>%
  mutate(discurso_limpo = gsub("[R|r]elator", "", discurso_limpo)) %>%
  mutate(discurso_limpo = gsub("Presidente", "", discurso_limpo)) %>%
  mutate(discurso_limpo = gsub("favor", "", discurso_limpo)) %>%
  mutate(discurso_limpo = gsub("contra", "", discurso_limpo)) %>%
  mutate(discurso_limpo = gsub("Michel", "", discurso_limpo)) %>%
  mutate(discurso_limpo = gsub("Temer", "", discurso_limpo)) %>%
  mutate(discurso_limpo = gsub("[D|d]eputad[o|a](s){0,1}", "", discurso_limpo)) %>%
  mutate(discurso_limpo = gsub("parecer", "", discurso_limpo))

tokens_discursos <- discursos %>%
  unnest_tokens(palavra, discurso_limpo)

p_sim <- tokens_discursos %>%
  filter(voto == "sim") %>%
  anti_join(stop_words_br) %>%
  count(palavra, sort = T) %>%
  mutate(palavra = reorder(palavra, n)) %>%
  top_n(30) %>%
  ggplot(aes(x = palavra, y = n)) + 
  geom_bar(stat = "identity", fill = "#2b83ba") + 
  labs(y = "Número de usos", x = NULL) + 
  coord_flip() + 
  theme_plex() +
  ggtitle('Palavras mais usadas pelo voto "sim"', subtitle = "251 votos contra a denúncia") + 
  theme(
    axis.title = element_text(family="IBMPlexSans-Bold"),
    panel.grid.major.x = element_line(), 
    plot.caption = element_text(size = 8, colour = "darkgrey"), 
    plot.title = element_text(size = 12),
    plot.subtitle = element_text(size = 10)
  )

p_nao <- tokens_discursos %>%
  filter(voto == "não") %>%
  anti_join(stop_words_br) %>%
  count(palavra, sort = T) %>%
  mutate(palavra = reorder(palavra, n)) %>%
  top_n(30) %>%
  ggplot(aes(x = palavra, y = n)) + 
  geom_bar(stat = "identity", fill = "#d7191c") + 
  labs(y = "Número de usos", x = "Palavra") + 
  coord_flip() + 
  theme_plex() +
  ggtitle('Palavras mais usadas pelo voto "não"', subtitle = "233 votos à favor da denúncia") + 
  theme(
    axis.title = element_text(family="IBMPlexSans-Bold"),
    panel.grid.major.x = element_line(), 
    plot.caption = element_text(size = 8, colour = "darkgrey"),
    plot.title = element_text(size = 12),
    plot.subtitle = element_text(size = 10)
  )

p_usos <- grid.arrange(p_nao, p_sim, ncol = 2)
ggsave(p_usos, file = "~/Desktop/p_usos.png", width = 11, height = 7)

tokens_discursos_contagem <- discursos %>%
  unnest_tokens(palavra, discurso_contagem)

maioria_partido <- discursos %>%
  group_by(partido) %>%
  summarise(maioria = names(sort(-table(voto)))[1])

media_de_palavras <- nrow(tokens_discursos_contagem)/nrow(discursos)

total_por_partido <- tokens_discursos_contagem %>%
  group_by(partido) %>%
  count(sort = TRUE) %>%
  left_join(discursos %>%
              group_by(partido) %>%
              summarise(total = n())) %>%
  mutate(freq = n / total) %>%
  ungroup() %>%
  right_join(maioria_partido) %>%
  mutate(partido = reorder(partido, freq)) %>%
  arrange(desc(freq))

p_media_palavras <- ggplot(aes(x = partido, y = freq), data = total_por_partido) + 
  geom_bar(aes(fill = maioria), stat = "identity") + 
    labs(x = "Partido", y = "Número médio de palavras por voto") + 
    geom_hline(yintercept = media_de_palavras, col = "black", linetype = "dotted") + 
    annotate("text", x = 4.5, y = 29, label = "Cada deputado falou\n em média 21 palavras.", family="IBMPlexSans-Italic") + 
  ggtitle("Quantas palavras os deputados usaram?") + 
  coord_flip() +
  theme_plex() + 
    scale_fill_manual(name = "Maioria", values = c("#d7191c", "#2b83ba")) + 
    theme(
      axis.title = element_text(family="IBMPlexSans-Bold"),
      panel.grid.major.x = element_line(), 
      plot.caption = element_text(size = 8, colour = "darkgrey"),
      plot.title = element_text(size = 12),
      plot.subtitle = element_text(size = 10)
    )
  
ggsave(p_media_palavras, file = "~/Desktop/p_media_palavras.png", width = 6, height = 6)
#right_join(select(discursos, partido, deputado, voto)) %>%

palavras_por_deputado <- discursos %>%
  unnest_tokens(palavra, discurso_contagem) %>%
  group_by(deputado) %>%
  summarise(n_palavras = n())

discursos %>%
  left_join(palavras_por_deputado) %>%
  filter(n_palavras >= 40) %>%
  mutate(deputado = reorder(deputado, n_palavras)) %>%
  ggplot(aes(x = deputado, y = n_palavras)) +
  geom_bar(aes(fill = voto), stat = "identity") +
  coord_flip() + 
  theme_plex()

discursos <- discursos %>%
  left_join(palavras_por_deputado) %>%
  filter(voto != "abstenção") %>%
  group_by(voto) %>%
  arrange(ordem) %>%
  mutate(media = rollmean(n_palavras, 10, na.pad = T)) %>%
  ungroup() %>%
  mutate(resultado = case_when(
    ordem < 136 & voto == "não" ~ "indefinido",
    ordem < 157 & voto == "sim" ~ "indefinido",
    TRUE ~ "definido"))

p_medias <- discursos %>%  
  ggplot(aes(
    x = ordem,
    y = media,
    group = voto,
    colour = voto
  )) +
  geom_line() +
  geom_line(
    data = filter(discursos, resultado == "definido"),
    aes(
      x = ordem,
      y = media,
      group = voto,
      colour = voto
    ), 
    size = 1.2
  ) + 
  labs(x = "Ordem de chamada (por tipo de voto)", y = "Média acumulada") + 
  geom_vline(xintercept = 157, linetype = "dotted") + 
  geom_vline(xintercept = 136, linetype = "dotted") + 
  scale_colour_manual(name = "Voto", values = c("#d7191c", "#2b83ba")) + 
  annotate("text", x = 179, y = 6, label = 'Resultado definido\n(157 votos "sim")', family="IBMPlexSans-Italic") + 
  annotate("text", x = 253, y = 14, label = "251 votos", family="IBMPlexSans-Bold") + 
  annotate("text", x = 241, y = 29, label = "233 votos", family="IBMPlexSans-Bold") + 
  theme_plex() +
  theme(
    axis.title = element_text(family="IBMPlexSans-Bold"),
    panel.grid.major.x = element_line(), 
    plot.caption = element_text(size = 8, colour = "darkgrey"),
    plot.title = element_text(size = 12),
    plot.subtitle = element_text(size = 10)
  )
ggsave(p_medias, file = "~/Desktop/p_medias.png", width = 10, height = 3)

bigram_discursos <- discursos %>%
  unnest_tokens(bigram, discurso_contagem, token = "ngrams", n = 2) %>%
  separate(bigram, c("palavra1", "palavra2"), sep = " ") %>%
  filter(!palavra1 %in% stop_words_br$palavra) %>%
  filter(!palavra2 %in% stop_words_br$palavra) %>%
  group_by(palavra1, palavra2) %>%
  count()

bigram_graph <- bigram_discursos %>%
  filter(n > 3) %>%
  graph_from_data_frame()

set.seed(2017)

p_graph <- ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) + 
  xlab(NULL) + 
  ylab(NULL) +
  theme_plex() + theme(
    panel.grid.major = element_blank(),
    axis.text = element_blank()
  )

ggsave(p_graph, file = "~/Desktop/p_graph.png", width = 8, height = 8)

stop_words_br <- structure(list(palavra = c("porque", "bem", "aqui", "vai", "neste", "neste", "deste", "desta",  "de", "a", "o", "que", "e", "do", 
                                            "da", "em", "um", "para", "é", "com", "uma", "os", "no", 
                                            "se", "na", "por", "mais", "as", "dos", "como", "mas", "foi", 
                                            "ao", "ele", "das", "tem", "à", "seu", "sua", "ou", "ser", "quando", 
                                            "muito", "há", "nos", "já", "está", "eu", "também", "só", 
                                            "pelo", "pela", "até", "isso", "ela", "entre", "era", "depois", 
                                            "sem", "mesmo", "aos", "ter", "seus", "quem", "nas", "me", "esse", 
                                            "eles", "estão", "você", "tinha", "foram", "essa", "num", "nem", 
                                            "suas", "meu", "às", "minha", "têm", "numa", "pelos", "elas", 
                                            "havia", "seja", "qual", "será", "nós", "tenho", "lhe", "deles", 
                                            "essas", "esses", "pelas", "este", "fosse", "dele", "tu", "te", 
                                            "vocês", "vos", "lhes", "meus", "minhas", "teu", "tua", "teus", 
                                            "tuas", "nosso", "nossa", "nossos", "nossas", "dela", "delas", 
                                            "esta", "estes", "estas", "aquele", "aquela", "aqueles", "aquelas", 
                                            "isto", "aquilo", "estou", "está", "estamos", "estão", "estive", 
                                            "esteve", "estivemos", "estiveram", "estava", "estávamos", "estavam", 
                                            "estivera", "estivéramos", "esteja", "estejamos", "estejam", 
                                            "estivesse", "estivéssemos", "estivessem", "estiver", "estivermos", 
                                            "estiverem", "hei", "há", "havemos", "hão", "houve", "houvemos", 
                                            "houveram", "houvera", "houvéramos", "haja", "hajamos", "hajam", 
                                            "houvesse", "houvéssemos", "houvessem", "houver", "houvermos", 
                                            "houverem", "houverei", "houverá", "houveremos", "houverão", 
                                            "houveria", "houveríamos", "houveriam", "sou", "somos", "são", 
                                            "era", "éramos", "eram", "fui", "foi", "fomos", "foram", "fora", 
                                            "fôramos", "seja", "sejamos", "sejam", "fosse", "fôssemos", 
                                            "fossem", "for", "formos", "forem", "serei", "será", "seremos", 
                                            "serão", "seria", "seríamos", "seriam", "tenho", "tem", "temos", 
                                            "tém", "tinha", "tínhamos", "tinham", "tive", "teve", "tivemos", 
                                            "tiveram", "tivera", "tivéramos", "tenha", "tenhamos", "tenham", 
                                            "tivesse", "tivéssemos", "tivessem", "tiver", "tivermos", "tiverem", 
                                            "terei", "terá", "teremos", "terão", "teria", "teríamos", 
                                            "teriam")), .Names = "palavra", row.names = c(NA, -220L), class = "data.frame")

theme_plex <- function(base_size = 14,
                       strip_text_size = 12,
                       strip_text_margin = 5,
                       subtitle_size = 13,
                       subtitle_margin = 10,
                       plot_title_size = 18,
                       plot_title_margin = 10,
                       ...) {
  ret <- ggplot2::theme_minimal(base_family = "IBMPlexSans",
                                base_size = base_size, ...)
  ret$strip.text <- ggplot2::element_text(hjust = 0, size=strip_text_size,
                                          margin=margin(b=strip_text_margin),
                                          family="IBMPlexSans-Medium")
  ret$plot.subtitle <- ggplot2::element_text(hjust = 0, size=subtitle_size,
                                             margin=margin(b=subtitle_margin),
                                             family="IBMPlexSans")
  ret$plot.title <- ggplot2::element_text(hjust = 0, size = plot_title_size,
                                          margin=margin(b=plot_title_margin),
                                          family="IBMPlexSans-Bold")
  ret$panel.grid.minor = ggplot2::element_blank()
  ret$panel.grid.major.x = ggplot2::element_blank()
  ret$plot.margin = ggplot2::unit(c(1,1, 1, 1), "lines")
  ret
}
