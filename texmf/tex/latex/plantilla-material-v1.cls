\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{plantilla-material-v1}[2024/9/21 v1.0 Plantilla para guías]

\PassOptionsToClass{10pt,spanish,oneside,twocolumn,letterpaper}{article}
\LoadClass{article}

\providecommand{\colegio}{COLEGIO}
\providecommand{\asignatura}{Departamento de Matemática}
\providecommand{\titulo}{TITULO}
\providecommand{\subtitulo}{SUBTITULO}
\providecommand{\nombre}{Prof. Fernando Halabi}
\providecommand{\correo}{correo@ASDAS.cl}

\RequirePackage{expl3}
\RequirePackage{luacode}
\RequirePackage[spanish,es-noshorthands]{babel} 
\selectlanguage{spanish} 
%\usepackage[fontsize=9pt]{fontsize}
\RequirePackage{icomma}
\RequirePackage{calc}
\RequirePackage{ifthen}
\RequirePackage{xparse}
\RequirePackage{hyperref}
\RequirePackage{lipsum}
\RequirePackage{tikz}
\RequirePackage{varwidth}
\RequirePackage{pgfplots}
\usepgfplotslibrary{statistics}
\pgfplotsset{compat=newest}
\usepgflibrary {fpu}
\usetikzlibrary{positioning,calc,math,backgrounds,%
shapes.symbols,shapes.geometric,shapes.misc,shapes.callouts,%
decorations.pathreplacing,calligraphy,arrows.meta,%
cd,intersections,patterns,patterns.meta,angles,through}
\RequirePackage{pgfornament}

\pgfkeys{/pgf/number format/.cd,
  set decimal separator={,},set thousands separator={\,}}
%\pgfkeys{/pgf/number format/.cd,fixed,fixed zerofill,precision=3}

\RequirePackage[most]{tcolorbox}
\RequirePackage{adjustbox}
\RequirePackage{tabularray}
\RequirePackage{array}
\RequirePackage{environ}
\RequirePackage{xcolor}
\RequirePackage{varwidth}
\UseTblrLibrary{siunitx,varwidth,diagbox,functional}
\sisetup{round-precision=3,round-mode=places,output-decimal-marker={,},
  mode = text, text-font-command = \normalsize}
\RequirePackage{enumitem}
\RequirePackage{tasks}
\RequirePackage{fontawesome5}
\RequirePackage{lastpage}
\RequirePackage[
  paper=letterpaper,
  top=2cm,
  bottom=2cm,
  left=1cm,
  right=1cm,
  headheight=2cm,
  headsep=15pt,
  footskip=20pt,
  %showframe
]{geometry}
\RequirePackage[skip=10pt, indent=0pt]{parskip}

\RequirePackage{fancyhdr}
% \setlength{\headheight}{26pt}
% \setlength{\headsep}{6pt}
\pagestyle{fancy}
\fancyhfoffset[H]{0pt} 
\fancyhead[C]{}
\fancyhead[L]{\ifthenelse{\thepage=1}{%
  \footnotesize{\colegio\\{\asignatura}\ifthenelse{\isundefined{\curso}}{}{- {\curso}} }
  }{}
}
\fancyhead[R]{}
\fancyfoot[L,C]{}
\fancyfoot[R]{\footnotesize{Página {\thepage} de {\pageref*{LastPage}}}}


\RequirePackage{notomath}
\setmonofont{NotoMono-Regular}[
  Path = /Users/fenho/Library/Fonts/,
  Extension =  .ttf,
  Ligatures = TeX,
  Scale=MatchLowercase
]

\RequirePackage[compact,medium,sc,bf,sf]{titlesec}
\renewcommand{\thesection}{\Roman{section}}
\titleformat{\section}{\sffamily\scshape\bfseries\large}{\thesection.}{1.5em}{}


\NewTasksEnvironment[label={\tcbox[colback=black!60, colframe=black!60, coltext=white,
   on line, boxsep=0pt, left=0pt, right=0pt, top=1pt, bottom=1pt,
   width=1cm]{\makebox[\widthof{22}][c]{\sffamily\bfseries\footnotesize\arabic*}}},
label-width=15pt,item-indent=30pt,label-offset=10pt,resume]{ejercicios}[\ejercicio](2)

\NewDocumentEnvironment{lista}{O{}+b}%
{%
    \begin{itemize}[nosep,topsep=0pt,#1]
        #2
}{%
    \end{itemize}
}

%% Setting values
\newlength{\myitemsep}
\setlength{\myitemsep}{3pt}
\newlength{\mytopsep}
\setlength{\mytopsep}{5pt}

\NewDocumentEnvironment{preguntas}{O{}+b}%
{%
    \newcommand{\pregunta}{\item}
    \begin{minipage}{\linewidth}
      \vspace*{\mytopsep}
      \begin{enumerate}[label={{\itshape\alph*})},
        labelsep=10pt,leftmargin=0pt,labelwidth={\widthof{{\itshape a})}},
        itemindent={\widthof{{\itshape a})}}+10pt,labelindent=0pt,itemsep=\myitemsep,topsep=\mytopsep,
        listparindent=0pt,#1]
          #2      
}{%
    \end{enumerate}
  \end{minipage}
}

\NewDocumentCommand{\figuraDoble}{mmO{10pt}}{
  \begin{tikzpicture}
    \matrix[ampersand replacement=\&]{
      \node{#1}; \&[#3] \node{#2}; \\
    };
  \end{tikzpicture}
}

\NewDocumentCommand{\figuraTriple}{mmmO{10pt}O{0pt}}{
  \begin{tikzpicture}
    \matrix[ampersand replacement=\&]{
      \node{#1}; \&[#4] \node{#2}; \&[#4+#5] \node{#3}; \\
    };
  \end{tikzpicture}
}


\NewDocumentEnvironment{importante}{+b}{%
  \if@firstcolumn
    \begin{tcolorbox}[blank,borderline west={4pt}{-10pt}{black},fontupper=\raggedright]
  \else
    \begin{tcolorbox}[blank,borderline east={4pt}{-10pt}{black},fontupper=\raggedright]
  \fi
      #1
}{%
    \end{tcolorbox}
}

\AtBeginDocument{%
	\hypersetup{%
		pdftitle = {\titulo - \subtitulo},
    pdfauthor = {\nombre},
    pdfcreator = {Vscode + LuaTex}
	}%
  \begin{center}
    \sffamily\Large {\scshape\bfseries \titulo} - \subtitulo
  \end{center}
  \raggedright
  \selectcolormodel{gray}
}