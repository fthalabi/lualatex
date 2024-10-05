#!/usr/bin/env Rscript

suppressMessages({
    library("optparse")
    library("tidyverse")
    library("showtext")
    library("janitor")
})

option_list <- list(
    make_option(c("-o", "--out"), type = "character",
        default = "output_file",
        help = "base name of the output file", metavar = "character"),
    make_option(c("-n", "--number"), type = "character", default = "1",
        help = "numberical suffix",
        metavar = "character")
)
opt_parser <- OptionParser(option_list = option_list)
opt <- parse_args(opt_parser)

font_add("noto", #
    regular = "NotoSansMerged-Condensed.ttf", #
    bold = "NotoSans-CondensedBold.ttf", #
    italic = "NotoSans-CondensedItalic.ttf")
showtext_auto()
theme_set(theme_minimal(base_size = 10, base_family = "noto"))

tabla_a_latex <- function(x, config = "shahsdhasdh") {
    resultado_latex <- ""
    colnames(x)  %>% as.vector()  %>% as.character()  %>% 
        paste(., collapse = "&")  %>% 
        paste(resultado_latex,., "\\\\\n") -> resultado_latex
    for (i in seq_len(nrow(x))) {
        x[i,] %>% as.vector() %>% as.character()  %>% 
            paste(., collapse = "&")  %>% 
            paste(resultado_latex,., "\\\\\n") -> resultado_latex
    }
    paste(paste("\\begin{center}\\begin{tblr}{",
        config,"}\n",collapse = "", sep = ""),
        resultado_latex,"\\end{tblr}\\end{center}")
}

output_name <- paste0(opt$out, "_", opt$number, ".tex")

paste0(r"(\providecommand{\curso}{Séptimo Básico}
\providecommand{\colegio}{Colegio Divina Pastora}
\providecommand{\tituloDocumento}{Guía 3}
\providecommand{\subtituloDocumento}{Tabla de frecuencias (\# )",
opt$number,
r"()}
\documentclass{cdplf-prueba}
\begin{document}
\subsection{}

Use los datos a continuación para llenar la tabla de frecuencias.
)") %>% 
    cat(.,
        file = output_name, sep = "\n", append = FALSE)

datos <- rnorm(40,8,3) %>% floor() %>% .[. >= 0]
datos %>% 
    paste(., collapse = " \\hspace{4pt}\\textbullet\\hspace{4pt} ")  %>% 
    paste("\\underline{Datos:} \\hspace{4pt}", ., collapse = "")  %>% 
    cat(.,
        file = output_name, sep = "\n", append = TRUE)

datos %>% 
    tabyl() %>% 
    mutate("Frecuencia" = "", 
        "Probabilidad" = "", 
        "Frecuencia Acumulada" = "",
        "Probabilidad Acumulada" = "") %>%
    select(".",
        "Frecuencia",
        "Probabilidad",
        "Frecuencia Acumulada",
        "Probabilidad Acumulada") %>%
    tabla_a_latex(.,config = paste(
        "colspec={ccccc}",
        "hlines",
        "vlines",
        "hline{2,Z} = {1}{-}{}",
        "hline{2,Z} = {2}{-}{}",
        "row{even}={black!10}",
        "rowsep=0pt",
        sep = ","
    )) %>%
    cat(.,
        file = output_name, sep = "\n", append = TRUE)

ymax <- datos %>% tabyl %>% select("n") %>% max
ggplot(data = tibble(Valores = datos)) +
        geom_blank(aes(x = Valores), stat = "count") +
        scale_x_continuous(
            breaks = 0:max(datos),
            limits = c(-1, max(datos) + 1)) +
        scale_y_continuous(
            breaks = 0:ymax,
            limits = c(0, ymax)) +
        labs(y = "Frecuencia",
            title = "Gráfico de barras") -> empty_plot

empty_plot_filename <- paste0("grafico_vacio_", opt$number, ".pdf")
ggsave(plot = empty_plot,
        filename = empty_plot_filename,
        device = cairo_pdf,
        units = "cm",
        width = 16,
        height = 8)

paste0(r"(\subsection{}

Haga un gráfico de barras usando las frecuencias de la tabla anterior.
\begin{center}\includegraphics{)", empty_plot_filename, r"(}\end{center}
)") %>%
    cat(.,
        file = output_name, sep = "\n", append = TRUE)

r"(\section*{Soluciones}
\setcounter{subsection}{0}
\subsection{}
)" %>% 
    cat(.,
        file = output_name, sep = "\n", append = TRUE)

datos %>% 
    tabyl() %>% 
    mutate("Frecuencia" = n, 
        "Probabilidad" = (percent  %>% round(digits = 3)), 
        "Frecuencia Acumulada" = cumsum(n),
        "Probabilidad Acumulada" = cumsum(Probabilidad)) %>%
    select(".",
        "Frecuencia",
        "Probabilidad",
        "Frecuencia Acumulada",
        "Probabilidad Acumulada") %>%
    tabla_a_latex(.,config = paste(
        "colspec={ccccc}",
        "hlines",
        "vlines",
        "hline{2,Z} = {1}{-}{}",
        "hline{2,Z} = {2}{-}{}",
        "row{even}={black!10}",
        sep = ","
    )) %>%
    cat(.,
        file = output_name, sep = "\n", append = TRUE)

ggplot(data = tibble(Valores = datos)) +
    geom_bar(aes(x = Valores)) +
    scale_x_continuous(
        breaks = 0:max(datos),
        limits = c(-1, max(datos) + 1)) +
    scale_y_continuous(breaks = 0:10) +
    labs(y = "Frecuencia", title = "Gráfico de barras") -> bar_plot

bar_plot_filename <- paste0("grafico_barras_", opt$number, ".pdf")
ggsave(plot = bar_plot,
        filename = bar_plot_filename,
        device = cairo_pdf,
        units = "cm",
        width = 16,
        height = 9)

paste0(r"(\subsection{}
\begin{center}\includegraphics{)", bar_plot_filename, r"(}\end{center}
\end{document})") %>%
    cat(.,
    file = output_name, sep = "\n", append = TRUE)


