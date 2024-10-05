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
\providecommand{\tituloItem}{Parte}
\documentclass{cdplf-prueba}
\begin{document}
\subsection{}

Use los datos a continuación para llenar la tabla de frecuencias.
)") %>% 
    cat(.,
        file = output_name, sep = "\n", append = FALSE)

datos <- rnorm(55,11,4) %>% floor() %>% .[. >= 0]
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
            title = "Gráfico de barras") + 
        theme(
            panel.grid.major = element_line(colour = "grey50"), 
            panel.grid.minor = element_line(colour = "grey60")
        ) -> empty_plot

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

paste0(r"(\subsection{}
Usando los resultados anteriores, responda las siguientes preguntas
\begin{tasks}[label={\tcbox[colback=black!60, colframe=black!60, coltext=white, on line, boxsep=0pt, left=3pt, right=3pt, top=2pt, bottom=2pt]{\sffamily\bfseries\alph*}},
item-indent=1.2cm,column-sep=20pt,label-offset=0.3cm,label-width=15pt,after-item-skip=10pt]
    \task! ¿Cuánto vale la media (promedio) de los datos? ¿Qué significa que tenga este valor? \begin{lineas}[height=1.5cm]\end{lineas}
    \task! ¿Cuánto vale la mediana de los datos? ¿Qué significa que tenga este valor? \begin{lineas}[height=1.5cm]\end{lineas}
    \task! ¿Cuál es el rango de los datos? ¿Qué significa que tenga este valor? \begin{lineas}[height=1.5cm]\end{lineas}
    \task! ¿Cuánto vale el primer cuartil de los datos? ¿Qué significa que tenga este valor? \begin{lineas}[height=1.5cm]\end{lineas}
    \task! ¿Cuánto vale el tercer cuartil de los datos? ¿Qué significa que tenga este valor? \begin{lineas}[height=1.5cm]\end{lineas}
    \task! ¿A qué valor corresponde el percentil del 90\%? ¿Qué significa que tenga este valor? \begin{lineas}[height=1.5cm]\end{lineas}
\end{tasks})") %>%
    cat(.,
        file = output_name, sep = "\n", append = TRUE)

t <- tibble(
    group = "A",
    lower = quantile(datos, 0.25, type = 1, names = FALSE),
    middle = quantile(datos, 0.5, type = 1, names = FALSE),
    upper = quantile(datos, 0.75, type = 1, names = FALSE),
    RIQ = upper - lower,
    min = max(lower - 1.5*RIQ,min(datos)),
    max = min(upper + 1.5*RIQ,max(datos))
)
ggplot(data = tibble(d=datos)) + 
    geom_blank(aes(d)) + 
    labs(x="Datos",title = "Diagrama de caja") + 
    scale_x_continuous(
        breaks = seq(
            from = min(datos) - 1,
            to = max(datos) + 1,
            by = 1
        ),
        limits = c(as.numeric(t[1,"min"])-1,as.numeric(t[1,"max"])+1)
    ) +
    theme(
        panel.grid.major = element_line(colour = "grey50"), 
        panel.grid.minor = element_line(colour = "grey60")
    ) -> diagrama_caja_vacio

diagrama_caja_vacio_filename <- paste0("diagrama_caja_vacio_", opt$number, ".pdf")
ggsave(plot = diagrama_caja_vacio,
        filename = diagrama_caja_vacio_filename,
        device = cairo_pdf,
        units = "cm",
        width = 16,
        height = 4)

paste0(r"(\subsection{}

Haga un diagrama de caja usando los datos anteriores.
\begin{center}\includegraphics{)", diagrama_caja_vacio_filename, r"(}\end{center}
)") %>%
    cat(.,
        file = output_name, sep = "\n", append = TRUE)

r"(\newpage\section*{Soluciones}
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
    labs(y = "Frecuencia", title = "Gráfico de barras") + 
    theme(
        panel.grid.major = element_line(colour = "grey50"), 
        panel.grid.minor = element_line(colour = "grey60")
    ) -> bar_plot

bar_plot_filename <- paste0("grafico_barras_", opt$number, ".pdf")
ggsave(plot = bar_plot,
        filename = bar_plot_filename,
        device = cairo_pdf,
        units = "cm",
        width = 16,
        height = 9)

paste0(r"(\subsection{}
\begin{center}\includegraphics{)", bar_plot_filename, r"(}\end{center})") %>%
    cat(.,
    file = output_name, sep = "\n", append = TRUE)

paste0(r"(\subsection{}
\begin{tasks}[label={\tcbox[colback=black!60, colframe=black!60, coltext=white, on line, boxsep=0pt, left=3pt, right=3pt, top=2pt, bottom=2pt]{\sffamily\bfseries\alph*}},
item-indent=1.2cm,column-sep=20pt,label-offset=0.3cm,label-width=15pt,after-item-skip=10pt,item-format=\raggedright](2))",
r"(\task La media es )", round(mean(datos), digits = 3) ,r"(.
 Esto significa que los valores más frecuentes son los que están cercanos a )", 
 round(mean(datos), digits = 3) , r"(, y es donde también se encuentran las barras más altas 
 en el gráfico de barras.)", 
 r"(\task La mediana es )", quantile(datos, 0.5, type = 1, names = FALSE), r"(. 
 Esto significa que la mitad (50\%) de los datos tiene un valor menor 
 o igual a )", quantile(datos, 0.5, type = 1, names = FALSE), r"(.)", 
 r"(\task El rango de los datos es )", max(datos) - min(datos), r"(. Esto 
 significa que la distancia entre el máximo ()", max(datos), r"() y el mínimo ()",
 min(datos), r"() de los datos es )", max(datos) - min(datos), r"(.)",
 r"(\task El primer cuartil es )", quantile(datos, 0.25, type = 1, names = FALSE),
 r"(. Esto significa que un cuarto de los datos (25\%) tiene un valor 
 menor o igual a )", quantile(datos, 0.25, type = 1, names = FALSE), ".",
 r"(\task El tercer cuartil es )", quantile(datos, 0.75, type = 1, names = FALSE),
 r"(. Esto significa que tres cuartos de los datos (75\%) tiene un valor menor
 o igual a )", quantile(datos, 0.75, type = 1, names = FALSE), ".",
 r"(\task El percentil del 90\% es )", quantile(datos, 0.9, type = 1, names = FALSE),
 r"(. Esto significa que el 90\% de los datos tiene un valor menor o igual a )",
 quantile(datos, 0.9, type = 1, names = FALSE), ".",
 r"(\end{tasks})") %>% 
    cat(.,
        file = output_name, sep = "\n", append = TRUE)


ggplot(data = t) + 
    geom_boxplot(
        mapping = aes(
            y = group,
            xmin = min,
            xlower = lower,
            xmiddle = middle,
            xupper = upper,
            xmax = max
        ),
        stat = "identity", 
        fill = "grey70",
        size = 1
    ) + 
    labs(x="Datos",title = "Diagrama de caja") + 
    scale_x_continuous(
        breaks = seq(
            from = min(datos) - 1,
            to = max(datos) + 1,
            by = 1
        ),
        limits = c(as.numeric(t[1,"min"])-1,as.numeric(t[1,"max"])+1)
    ) +
    #ylim(c(-1,1)) +
    theme(
        panel.grid.major = element_line(colour = "grey50"), 
        panel.grid.minor = element_line(colour = "grey60"),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.title.y = element_blank()
    ) -> diagrama_caja

diagrama_caja_filename <- paste0("diagrama_caja_", opt$number, ".pdf")
ggsave(plot = diagrama_caja,
        filename = diagrama_caja_filename,
        device = cairo_pdf,
        units = "cm",
        width = 16,
        height = 4)
    
paste0(r"(\subsection{}
\begin{center}\includegraphics{)", diagrama_caja_filename, r"(}\end{center}
\end{document})") %>%
    cat(.,
    file = output_name, sep = "\n", append = TRUE)

