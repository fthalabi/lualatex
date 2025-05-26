\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{srs}[2025/03/03 v1.0 Plantilla para SRS]

\PassOptionsToClass{12pt,oneside}{article}
\LoadClass{article}

\providecommand{\colegio}{School Rose School}
\providecommand{\asignatura}{Departamento de Matemática}
\providecommand{\titulo}{TITULO}
\providecommand{\subtitulo}{SUBTITULO}
\providecommand{\nombre}{Fernando Halabi A.}
\providecommand{\correo}{correo@ASDAS.cl}
\providecommand{\curso}{Curso}
\providecommand{\fecha}{XX de XXXXXX,XXXX}

\input{paquetes}

% Define variables for options
\newboolean{optionone}
\newboolean{optiontwo}
\newboolean{optionthree}
% Set default values
\setboolean{optionone}{false}
\setboolean{optiontwo}{false}
\setboolean{optionthree}{false}
% Declare options
\DeclareOption{paginavacia}{\setboolean{optionone}{true}}
\DeclareOption{sinnombre}{\setboolean{optiontwo}{true}}
\DeclareOption{conautor}{\setboolean{optionthree}{true}}
% Handle unknown options
\DeclareOption*{\PackageWarning{yourclassname}{Unknown option '\CurrentOption'}}
% Process the options
\ProcessOptions\relax

\renewcommand{\thesection}{\Roman{section}}
\titleformat{\section}{\sffamily\bfseries\Large}{\thesection.}{1.5em}{}
\titlespacing*{\section}{0pt}{*10}{*5}

\titleformat{\subsection}{\sffamily\bfseries\large}{\thesubsection}{1.5em}{}
\titlespacing*{\subsection}{0pt}{*4}{*1.5}

\geometry{
  paperheight=33cm,
  paperwidth=21.6cm,
  top=3cm,
  bottom=2.5cm,
  left=2cm,
  right=2cm,
  headheight=2cm,
  headsep=0.5cm, %15
  footskip=0.5cm,
  marginparwidth=0pt,
  includemp=false,
  heightrounded,
  %showframe
}

\setlength{\marginparwidth}{0pt}
\setlength{\marginparsep}{0pt}

\ifthenelse{\boolean{optionone}}{\pagestyle{empty}}{\pagestyle{fancy}}

\fancyhfoffset[H]{0pt}
\fancyhead[C]{%
\ifnum\value{page}=1
\begin{tikzpicture}[baseline=-0.2cm]
  \node (A) {\includegraphics[width=1cm]{srs_logo.pdf}};
\end{tikzpicture}
\else
{\vspace*{10pt}\normalsize\sffamily{\bfseries\titulo} - \subtitulo}
\fi%
}
\fancyhead[L]{\ifnum\value{page}=1 \textit{Saint Rose School\\Departamento de Matemática}\fi}
\fancyhead[R]{\ifnum\value{page}=1 \textit{Sir: \nombre\\Curso: \curso}\fi}
\fancyfoot[L,C]{}
\fancyfoot[R]{\footnotesize{Página~\thepage~de~\pageref{LastPage}}}


\newtcolorbox{caja}[1][]{enhanced,colback=white,colframe=black,boxrule=1pt,attach boxed title to top left={yshift=-\tcboxedtitleheight/2,xshift=10pt},
boxed title style={colback=white},coltitle=black,height=30pt,#1}

\newtcolorbox{malla}[1][]{enhanced,inherit height,colback=white,colframe=black,boxrule=1pt,underlay={\begin{tcbclipinterior}
  \draw[help lines,black!20,step=5mm,yshift=0pt] (interior.south west) grid (interior.north east);\end{tcbclipinterior}},#1}

\newtcolorbox{respuesta}[1][]{enhanced,inherit height,colback=white,colframe=black,boxrule=1pt,underlay boxed title={\begin{tcbclipinterior}
  \draw[help lines,step=5mm] (interior.south west) grid[xstep=0] (interior.north east);\end{tcbclipinterior}},title=Respuesta,attach boxed title to top left={yshift=-\tcboxedtitleheight/2,xshift=10pt},
  boxed title style={colback=white},coltitle=black,#1}
      
\NewDocumentEnvironment{importante}{O{}+b}{
\begin{center}
\begin{tcolorbox}[borderline={1pt}{0pt}{black,dashed},colframe=white,colback=white,
  before skip=5mm,after skip=5mm,width=.9\linewidth, halign=flush left,#1]
  #2
}{
\end{tcolorbox}
\end{center}
}


% #1 left and right pad  
% #2 top and bottom pad  
% #3 width of internal box
% #4 character
\NewDocumentCommand{\mybox}{O{1pt}O{1pt}mm}{%
\tcbox[colback=black!60, colframe=black!60, coltext=white,%
    on line,boxsep=0pt, left=#2, right=#2, top=#2, bottom=#2,%
   width=1cm]{\makebox[#3][c]{#4}}
}

\NewDocumentEnvironment{partes}{O{}+b}%
{%
    \newcommand{\parte}{%
      \ifthenelse{\lengthtest{\dimexpr \pagegoal-\pagetotal\relax < 6cm}}{\clearpage}{}
      %\par
      %\needspace{5\baselineskip}%
      \item
    }
    \begin{enumerate}[label={\mybox[1pt][2pt]{\widthof{22}}{\sffamily\bfseries\arabic*}},
      wide, labelwidth=20pt, labelsep=15pt,itemindent=0pt,leftmargin=*, itemsep = 1ex,
      series=preguntas,resume=preguntas,#1]
        #2
}{%
    \end{enumerate}
}

%\NewTasksEnvironment[label={\mybox[1pt][2pt]{\widthof{M}}{\sffamily\bfseries\scshape\alph*}},
%label-width=\labelwidth,item-indent=30pt,
%label-offset=10pt,column-sep=10pt]{ejercicios}[\ejercicio](2)

%\NewTasksEnvironment[label={\mybox[1pt][2pt]{\widthof{22}}{\sffamily\bfseries\arabic*}},
%label-width=\labelwidth,item-indent=30pt,resume=true,
%label-offset=10pt,column-sep=10pt]{preguntas}[\pregunta](1)

\newcounter{mytaskcounter}

\NewDocumentCommand{\superbox}{m}{%
\ifthenelse{\value{mytaskcounter} < 100}{%
\tcbox[colback=black!60, colframe=black!60, coltext=white,%
  on line,boxsep=0pt, left=1pt, right=1pt, top=1pt, bottom=1pt,%
  width=1cm]{\makebox[\widthof{22}][c]{#1}}}{%
\tcbox[colback=black!60, colframe=black!60, coltext=white,%
  on line,boxsep=0pt, left=1pt, right=1pt, top=1pt, bottom=1pt,%
  width=1cm]{#1}}}

\NewTasksEnvironment[label=\superbox{\sffamily\small\bfseries\arabic*},
label-width=3em,item-indent=4em,resume=true,item-format={\raggedright},
label-offset=1em,column-sep=10pt,counter=mytaskcounter]{preguntas}[\pregunta](1)




%% Setting values
\newlength{\myitemsep}
\setlength{\myitemsep}{5pt}
\newlength{\mytopsep}
\setlength{\mytopsep}{10pt}

\NewTasksEnvironment[
  label={{\itshape\alph*})},
label-width=10pt,item-indent=20pt,label-offset=10pt,after-item-skip=\myitemsep,
before-skip=\mytopsep,after-skip=\mytopsep
]{alternativas}[\alternativa](1)

\NewDocumentEnvironment{vertical}{O{}+b}%
{%  
    \newline
    \newcommand{\alternativa}{\item}
    \begin{minipage}{\linewidth}
      \vspace*{\mytopsep}
      \begin{enumerate}[label={{\itshape\alph*})},
        labelsep=1em,leftmargin=2em,labelwidth=2em,
        labelindent=0pt,itemsep=\myitemsep,topsep=\mytopsep,
        listparindent=0pt,before=\raggedright,#1]
          #2      
}{%
    \end{enumerate}
    \vspace*{\mytopsep}
  \end{minipage}
}

\NewDocumentEnvironment{verticali}{O{}+b}%
{%
    \newline
    \newcommand{\alternativa}{\item}
    \begin{minipage}{\linewidth}
      \vspace*{\mytopsep}
      \begin{enumerate}[label={{\Roman*.}},
        labelsep=1em,leftmargin=5em,labelwidth=2em,
        labelindent=0pt,itemsep=\myitemsep,topsep=\mytopsep,
        listparindent=0pt,before=\raggedright,#1]
          #2      
}{%
    \end{enumerate}
    \vspace*{0.5\mytopsep}
  \end{minipage}
}

\NewDocumentEnvironment{verticaln}{O{}+b}%
{%  
    \newline
    \newcommand{\alternativa}{\item}
    \begin{minipage}{\linewidth}
      \vspace*{\mytopsep}
      \begin{enumerate}[label={(\arabic*)},
        labelsep=1em,leftmargin=5em,labelwidth=2em,
        labelindent=0pt,itemsep=\myitemsep,topsep=\mytopsep,
        listparindent=0pt,before=\raggedright,#1]
          #2      
}{%
    \end{enumerate}
    \vspace*{0.5\mytopsep}
  \end{minipage}
}


\NewDocumentEnvironment{horizontal}{O{}+b}%
{%
    \newcommand{\alternativa}{\item}
    \begin{enumerate*}[label={{\itshape\alph*})},itemjoin={\hspace{10pt} },#1]
        #2
}{%
    \end{enumerate*}
}

\NewDocumentCommand{\respuestas}{}{%
\begin{minipage}[b][\lineskip+15pt][t]{\linewidth}
  \centering
  \sffamily\Large {\bfseries Respuestas}
\end{minipage}
}

\NewDocumentEnvironment{alternativasgraficas}{O{}+b}{%
  \vspace*{\mytopsep}
  \NewDocumentCommand{\alternativa}{O{}}{\tcbitem[##1]}
  \begin{tcbitemize}[raster columns=2,raster equal height=rows,blank,lefttitle=0pt,
    %fonttitle=\itshape, halign=center,
    valign=center,leftupper=2em,top=-10pt,boxsep=0pt,
    breakable=false,nobeforeafter,
    coltitle=black,title={{\itshape\alphalph{\thetcbrasternum}})},#1]
      #2
}{%
  \end{tcbitemize}\phantom{.}\vspace*{\mytopsep}
}

\NewDocumentEnvironment{alternativasgraficasi}{O{}+b}{%
  \vspace*{\mytopsep}
  \NewDocumentCommand{\alternativa}{O{}}{\tcbitem[##1]}%
  \begin{tcbitemize}[raster columns=2,raster equal height=rows,blank,lefttitle=0pt,
    %fonttitle=\itshape, halign=center,
    valign=center,leftupper=2em,top=-10pt,boxsep=0pt,
    breakable=false,nobeforeafter,enhanced,before={\newline},after={hola\newline},
    coltitle=black,title={{\Romannum{\thetcbrasternum}}.},#1]
      #2
}{%
  \end{tcbitemize}\phantom{.}
}

\AtBeginDocument{%
  \pagenumbering{arabic}%
	\hypersetup{%
		pdftitle = {\titulo - \subtitulo},%
    pdfauthor = {\nombre},%
    pdfcreator = {Vscode + LuaTex}%
	}%
  \ifthenelse{\not\boolean{optionone}}{%
  \begin{center}%
    {\sffamily\Large\bfseries \titulo} - {\sffamily\Large\subtitulo}%
  \end{center}}{}%
  \ifthenelse{\boolean{optiontwo} \or \boolean{optionone}}{}{%
    \begin{tcbraster}[enhanced,raster columns=3,raster width=\linewidth,%
      raster column skip=3pt,raster force size=false,raster after skip=15pt,%
      raster before skip=10pt]%
      \begin{caja}[title={\sffamily\bfseries Nombre},height=35pt,add to width=4cm]%
      \end{caja}%
      \begin{caja}[title={\sffamily\bfseries Puntaje},height=35pt,add to width=-2cm,valign=bottom,halign=center]%
        \ifthenelse{\isundefined{\puntaje}}{}{\hspace*{30pt}/ \puntaje}%
      \end{caja}%
      \begin{caja}[title={\sffamily\bfseries Nota},height=35pt,add to width=-2cm]%
      \end{caja}%
    \end{tcbraster}%
    \hfill\textbf{\sffamily Fecha:} \fecha\hspace*{10pt}\\\vspace*{-30pt}%
  }%
}

\NewDocumentCommand{\negrita}{m}{{\sffamily\scshape\bfseries\large#1}}
\NewDocumentCommand{\f}{m}{\pgfmathprintnumber[fixed,fixed zerofill,precision=3,verbatim,use comma]{#1}}


\NewDocumentCommand{\separador}{O{5mm}}{
  \begin{center}
    \vspace*{#1}
    \begin{tikzpicture}
      \coordinate (A) at (0,0);
      \coordinate (B) at (8cm,0);
      \pgfornamentline[color=black!30]{A}{B}{1}{88}
    \end{tikzpicture}
    \vspace*{#1}
  \end{center}
}

\NewDocumentEnvironment{equation**}{b}{%
\newline
\begin{minipage}{\textwidth}
 \begin{equation*}
    #1
 \end{equation*}
\end{minipage}
}{}

%\NewDocumentEnvironment{mcaja}{O{}+b}{\newline%
%  \tcboxmath[borderline={1pt}{0pt}{black,dashed},colframe=white,colback=white,center,
%  before skip=-2pt,after skip=3pt,#1]%
%  {#2}
%}{}

\newcommand{\q}[1]{``#1''}

%\newtcolorbox{doteado}[1][]{enhanced,
%  borderline={1pt}{0pt}{black,dashed},colframe=white,colback=white,center,
%  before skip=-2pt,after skip=3pt,hbox,#1
%}

\NewDocumentEnvironment{doteado}{b}{%
\newline\noindent\hspace*{-4em}\begin{minipage}{\textwidth}
  \begin{tcolorbox}[enhanced,
    borderline={1pt}{0pt}{black,dashed},colframe=white,colback=white,center,
    before skip=-2pt,after skip=3pt,hbox]
      #1
  \end{tcolorbox}
\end{minipage}
}{}

%\NewDocumentEnvironment{centrado}{b}{%
%\newline\noindent\hspace*{-4em}\begin{minipage}{\textwidth}
%  \begin{tcolorbox}[enhanced,blank,center,
%    before skip=-2pt,after skip=3pt,hbox,top=7pt,bottom=7pt]
%      #1
%  \end{tcolorbox}
%\end{minipage}
%}{}

\newtcolorbox{centrado}[1][]{enhanced,blank,nobeforeafter,halign=center,valign=center,
top=7pt,bottom=7pt,breakable=false,before={\newline},left skip=-4em,
before upper={\tikzset{baseline=(current bounding box.center)}},
after upper={\tikz[overlay] \node[xshift=3em] {#1};}}

%\NewDocumentEnvironment{centrado}{b}{%
%\newline\begin{minipage}{\textwidth}
%    #1
%\end{minipage}
%}{}




\newtcolorbox{aviso}[1][]{enhanced,
  boxrule=0.4pt,left=5mm,right=2mm,top=2mm,bottom=2mm,
  %borderline={1pt}{0pt}{black,dashed},
  %colframe=white,colback=white,
  before skip=5mm,after skip=5mm,width=.9\linewidth, halign=flush left,
  enlarge left by=0.05\linewidth,
  colback=black!7,
  colframe=black!50,
  sharp corners,rounded corners=southeast,arc is angular,arc=3mm,
  underlay={%
    \path[fill=tcbcolback!80!black] ([yshift=3mm]interior.south east)--++(-0.4,-0.1)--++(0.1,-0.2);
    \path[draw=tcbcolframe,shorten <=-0.05mm,shorten >=-0.05mm] ([yshift=3mm]interior.south east)--++(-0.4,-0.1)--++(0.1,-0.2);
    \path[fill=black!50,draw=none] (interior.south west) rectangle node[white]{\Huge\bfseries !} ([xshift=4mm]interior.north west);
    },
  drop fuzzy shadow,#1}

% 2. Redefine \@maketitle to include \sffamily
\makeatletter
\def\@maketitle{%
  \newpage
  \null
  \vskip 2em%
  \begin{center}%
  \let \footnote \thanks % Important for \thanks command functionality
  \sffamily % Apply sans-serif to the entire title block
    {\LARGE \@title \par}%
    \vskip 1.5em%
    {\large
      \lineskip .5em%
      \begin{tabular}[t]{c}%
        \@author
      \end{tabular}\par}%
    \vskip 1em%
    {\large \@date}%
  \end{center}%
  \par
  \vskip 1.5em}
\makeatother

% --- Redefine \maketitle ---
\let\OriginalMakeTitle\maketitle
\renewcommand{\maketitle}{%
  \OriginalMakeTitle% Call the original \maketitle command
  \thispagestyle{empty}% Add this command immediately after
  \pagestyle{empty}
}

%%%% CONTERS FOR FIGURES AND TABLES

\newcounter{myfigurecounter}
\newcommand{\fignueva}{%
    \refstepcounter{myfigurecounter}%
    \themyfigurecounter\xspace%
}

% 3. Define a command to just output the current counter value
\newcommand{\figactual}{%
    \themyfigurecounter\xspace%
}
\newcounter{mytablecounter}
\newcommand{\tabnueva}{%
    \refstepcounter{mytablecounter}%
    \themytablecounter\xspace%
}

% 3. Define a command to just output the current counter value
\newcommand{\tabactual}{%
    \themytablecounter\xspace%
}
