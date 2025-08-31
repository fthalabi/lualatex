\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{srs3}[2025/08/28 v3.0 Plantilla para SRS]

\PassOptionsToClass{12pt,oneside}{article}
\LoadClass{article}

\input{paquetes}

\ExplSyntaxOn

%% LOCAL VARIABLES
\tl_new:N \l_srs_fecha_option_tl
\tl_new:N \l_srs_titulo_tl
\tl_new:N \l_srs_subtitulo_tl
\tl_new:N \l_srs_curso_tl
\tl_new:N \l_srs_nombre_tl
\tl_new:N \l_srs_correo_tl
\bool_new:N \l_srs_borrador_bool
\bool_new:N \l_srs_connombre_bool
\int_new:N \l_srs_puntaje_int

%% GLOBAL VARIABLES
\tl_new:N \g_srs_fecha_formateada_tl
\tl_new:N \g_srs_titulo_tl
\tl_new:N \g_srs_subtitulo_tl
\tl_new:N \g_srs_curso_tl
\tl_new:N \g_srs_nombre_tl
\tl_new:N \g_srs_correo_tl
\bool_new:N \g_srs_borrador_bool
\bool_new:N \g_srs_connombre_bool
\int_new:N \g_srs_puntaje_int

\keys_define:nn { srs }
{
  fecha .tl_set:N = \l_srs_fecha_option_tl,
  titulo .tl_set:N = \l_srs_titulo_tl,
  titulo .initial:n = {TITULO},
  subtitulo .tl_set:N = \l_srs_subtitulo_tl,
  subtitulo .initial:n = {SUBTITULO},
  curso .tl_set:N = \l_srs_curso_tl,
  curso .initial:n = {CURSO},
  nombre .tl_set:N = \l_srs_nombre_tl,
  nombre .initial:n = {NOMBRE},
  correo .tl_set:N = \l_srs_correo_tl,
  correo .initial:n = {CORREO},
  borrador .bool_set:N = \l_srs_borrador_bool,
  con~nombre .bool_set:N = \l_srs_connombre_bool,
  puntaje .int_set:N = \l_srs_puntaje_int,
  puntaje .initial:n = {0},
}

\ProcessKeyOptions[srs]

%% COPIANDO PARA ACCECIBILIDAD EN EL DOCUMENTO 
\tl_gset_eq:NN \g_srs_titulo_tl \l_srs_titulo_tl
\tl_gset_eq:NN \g_srs_subtitulo_tl \l_srs_subtitulo_tl
\tl_gset_eq:NN \g_srs_curso_tl \l_srs_curso_tl
\tl_gset_eq:NN \g_srs_nombre_tl \l_srs_nombre_tl
\tl_gset_eq:NN \g_srs_correo_tl \l_srs_correo_tl
\bool_gset_eq:NN \g_srs_borrador_bool \l_srs_borrador_bool
\bool_gset_eq:NN \g_srs_connombre_bool \l_srs_connombre_bool
\int_gset_eq:NN \g_srs_puntaje_int \l_srs_puntaje_int

% Define your custom date format
% Style name: 'dayofmonth'
\DTMnewdatestyle{dayofmonth}{%
  \renewcommand{\DTMdisplaydate}[4]{%
    % #1=year, #2=month, #3=day, #4=day of week index
    \int_case:nn {##4} {
      {0} {Lunes}
      {1} {Martes}
      {2} {Miércoles}
      {3} {Jueves}
      {4} {Viernes}
      {5} {Sábado}
      {6} {Domingo}
    }
    \space~\pgfmathprintnumber{##3}~de~\DTMmonthname{##2},~##1
  }%
  \renewcommand{\DTMDisplaydate}{\DTMdisplaydate}%
}
% Apply your custom date style
\DTMsetdatestyle{dayofmonth}

%%% User-facing command. Now it's robust against empty input.
%%\NewExpandableDocumentCommand{\obtenerfecha}{m}
%% {
%%  % First, we test if the input string #1 is empty.
%%  \tl_if_empty:nTF { #1 } {}
%%   {
%%    \__fecha_split_and_process:w #1 \q_stop
%%   }
%% }
%%
%%\cs_new:Npn \__fecha_split_and_process:w #1 - #2 - #3 \q_stop
%% {
%%  % Rearrange to YYYY-MM-DD and pass to \DTMdate
%%  \DTMdate { #3-#2-#1 }
%% }
%%
%%\tl_if_blank:VTF \l_srs_fecha_option_tl {
%%  \tl_gset:Nx \g_srs_fecha_formateada_tl {\today}
%%}{
%%  \tl_gset:Nx \g_srs_fecha_formateada_tl {\obtenerfecha{\tl_use:N \l_srs_fecha_option_tl}}
%%}

% --- This is your main logic block, now using the new function ---
\tl_if_blank:VTF \l_srs_fecha_option_tl
  {
    % User did not provide the fecha option, use today's date.
    \tl_gset:Nx \g_srs_fecha_formateada_tl { \today }
  }
  {
    % User provided a date. Format it using our robust function.
    \tl_gset:Nx \g_srs_fecha_formateada_tl
      {
        \DTMdate { \tl_use:N \l_srs_fecha_option_tl }
      }
  }

%% PARA ACCESO EN DOCUMENTO

\NewDocumentCommand{\srsfecha}{}{
  \tl_use:N \g_srs_fecha_formateada_tl
}
\NewDocumentCommand{\srstitulo}{}{
  \tl_use:N \g_srs_titulo_tl
}
\NewDocumentCommand{\srssubtitulo}{}{
  \tl_use:N \g_srs_subtitulo_tl
}
\NewDocumentCommand{\srscurso}{}{
  \tl_use:N \g_srs_curso_tl
}
\NewDocumentCommand{\srsnombre}{}{
  \tl_use:N \g_srs_nombre_tl
}
\NewDocumentCommand{\srscorreo}{}{
  \tl_use:N \g_srs_correo_tl
}
\NewDocumentCommand{\ifborradorTF}{ m m }
{
  \bool_if:NTF \g_srs_borrador_bool { #1 } { #2 }
}
\NewDocumentCommand{\ifconnombreTF}{ m m }
{
  \bool_if:NTF \g_srs_connombre_bool { #1 } { #2 }
}
\NewDocumentCommand{\srspuntaje}{}{
  \int_use:N \g_srs_puntaje_int
}
\NewDocumentCommand{\ifpuntajeTF}{ m m }{
  \int_if_zero:nTF \g_srs_puntaje_int { #2 } { #1 }
}


%% TITLE EXPERIMENTAL
\bool_new:N \g_srs_saltar_bool 
\bool_gset_false:N \g_srs_saltar_bool
%%

\NewDocumentCommand{\cancelarsalto} {}
{
  \bool_gset_true:N \g_srs_saltar_bool
}

\NewDocumentCommand{\ifcancelarsaltoTF} { m m }
{
  \bool_if:NTF \g_srs_saltar_bool 
  { 
    #1
    \bool_gset_false:N \g_srs_saltar_bool
  } { #2 }
}
\ExplSyntaxOff

\renewcommand{\thesection}{\Roman{section}}
\titleformat{\section}{\sffamily\bfseries\Large}{\thesection.}{1.5em}{}[]
\titlespacing*{\section}{0pt}{4ex + 4ex - 3ex}{3ex + 1ex - 2ex}

\let\oldsection\section
% ...and redefine it.
\renewcommand{\section}{%
  \ifcancelarsaltoTF%
  {\titlespacing*{\section}{0pt}{0pt}{3ex + 1ex - 2ex}}%
  {\titlespacing*{\section}{0pt}{4ex + 4ex - 3ex}{3ex + 1ex - 2ex}}%
  \oldsection % Now, typeset the section as normal
}

\titleformat{\subsection}{\sffamily\bfseries\large}{\thesubsection}{1.5em}{}
\titlespacing*{\subsection}{0pt}{2ex + 1ex - 1ex}{1.5ex +0.5ex -0.5ex}

\let\oldsubsection\subsection
% ...and redefine it.
\renewcommand{\subsection}{%
  \ifcancelarsaltoTF%
  {\titlespacing*{\subsection}{0pt}{0pt}{*1.5}}%
  {\titlespacing*{\subsection}{0pt}{2ex + 1ex - 1ex}{1.5ex +0.5ex -0.5ex}}%
  \oldsubsection % Now, typeset the section as normal
}


\titleformat{\subsubsection}{\sffamily\bfseries}{\thesubsubsection}{1.5em}{}
\titlespacing*{\subsubsection}{1cm}{1ex + 0.5ex - 0.5ex}{1ex + 0.5ex - 0.5ex}

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

\setlength{\cftbeforesecskip}{2.0ex}
\setlength{\cftbeforesubsecskip}{1.0ex}
\renewcommand{\cfttoctitlefont}{\sffamily\Large\bfseries}
\renewcommand{\cftsecfont}{\sffamily\large}
\renewcommand{\cftsubsecfont}{\sffamily}
\renewcommand{\cftsecpagefont}{\sffamily}
\renewcommand{\cftsubsecpagefont}{\sffamily}
\tocloftpagestyle{empty}

\ifborradorTF{\pagestyle{empty}}{\pagestyle{fancy}}

\fancyhfoffset[H]{0pt}
\fancyhead[C]{%
\ifnum\value{page}=1
\begin{tikzpicture}[baseline=-0.2cm]
  \node (A) {\includegraphics[width=1cm]{srs_logo.pdf}};
\end{tikzpicture}
\else
{\vspace*{10pt}\normalsize\sffamily{\bfseries\srstitulo} - \srssubtitulo}
\fi%
}
\fancyhead[L]{\ifnum\value{page}=1 \textit{Saint Rose School\\Departamento de Matemática}\fi}
\fancyhead[R]{\ifnum\value{page}=1 \textit{Sir: \srsnombre\\Curso: \srscurso}\fi}
\fancyfoot[L,C]{}
\fancyfoot[R]{\footnotesize{Página~\thepage~de~\pageref{LastPage}}}

%%% EMPIEZA PARTE EXPERIMENTAL
% #1 left and right pad  
% #2 top and bottom pad  
% #3 character
\NewDocumentCommand{\contorno}{O{1pt}O{1pt}m}{%
\tcbox[colframe=black!60,on line,boxsep=0pt,colback=white,
  left=#1, right=#1, top=#2, bottom=#2]{#3}}

\newcounter{nproblema}
\setcounter{nproblema}{1}

\NewDocumentCommand{\superbox}{m}{%
\ifthenelse{\value{nproblema} < 100}{%
\tcbox[colback=black!60, colframe=black!60, coltext=white,%
  on line,boxsep=0pt, left=1pt, right=1pt, top=1pt, bottom=1pt,%
  width=1cm]{\makebox[\widthof{22}][c]{\sffamily\small\bfseries #1}}}{%
\tcbox[colback=black!60, colframe=black!60, coltext=white,%
  on line,boxsep=0pt, left=1pt, right=1pt, top=1pt, bottom=1pt,%
  width=1cm]{\sffamily\small\bfseries #1}}}

\NewDocumentEnvironment{preguntas}{O{}+b}{%
  \NewDocumentCommand{\pregunta}{O{}}{\tcbitem[##1,blankest,breakable=false,%
      fontupper=\onehalfspacing]%
    \superbox{\thenproblema}\stepcounter{nproblema} \hspace{1em}}
  \begin{tcbitemize}[raster columns=1,lefttitle=0pt,blankest,
    halign=flush left,
    valign=center,leftupper=2em,top=0pt,boxsep=0pt,
    breakable=false,raster before skip=10pt,raster after skip=30pt,
    #1]
      #2
}{%
  \end{tcbitemize}
}

\newtcolorbox{malla}[1][]{enhanced,inherit height,colback=white,colframe=black,boxrule=1pt,underlay={\begin{tcbclipinterior}
  \draw[help lines,black!25,step=5mm,yshift=0pt] (interior.south west) grid (interior.north east);\end{tcbclipinterior}},
  height=#1cm}

\newtcolorbox{respuesta}[1][]{enhanced,inherit height,colback=white,colframe=black,boxrule=1pt,underlay boxed title={\begin{tcbclipinterior}
  \draw[help lines,step=5mm] (interior.south west) grid[xstep=0] (interior.north east);\end{tcbclipinterior}},title=Respuesta,attach boxed title to top left={yshift=-\tcboxedtitleheight/2,xshift=10pt},
  boxed title style={colback=white},coltitle=black,height=#1cm}

\newtcolorbox{centrado}[1][]{enhanced,blank,nobeforeafter,halign=center,valign=center,
top=7pt,bottom=7pt,breakable=false,before={\newline},
before upper={\tikzset{baseline=(current bounding box.center)}},
after upper={\tikz[overlay] \node[xshift=3em] {#1};}}

%% Setting values
\newlength{\myitemsep}
\setlength{\myitemsep}{5pt}
\newlength{\mytopsep}
\setlength{\mytopsep}{10pt}

\NewTasksEnvironment[label={{\itshape\alph*})},%
label-width=2em,item-indent=3em,item-format={\raggedright},%
label-offset=1em]{alternativas}[\alternativa](1)

\NewDocumentEnvironment{alternativas*}{O{3}+b}{%
  \vspace*{\mytopsep}
  \NewDocumentCommand{\alternativa}{O{}}{\tcbitem[##1]}
  \begin{tcbitemize}[raster columns=#1,raster equal height=rows,blank,lefttitle=0pt,
    %fonttitle=\itshape, halign=center,
    valign=center,leftupper=2em,top=-10pt,boxsep=0pt,
    breakable=false,nobeforeafter, raster row skip=10pt,
    coltitle=black,title={{\itshape\alphalph{\thetcbrasternum}})}]
      #2
}{%
  \end{tcbitemize}
}

\NewTasksEnvironment[label={{\Roman*.}},
label-width=2em,item-indent=6em,item-format={\raggedright},%
label-offset=1em]{opciones}[\opcion](1)

\NewTasksEnvironment[label={(\arabic*)},
label-width=2em,item-indent=6em,item-format={\raggedright},%
label-offset=1em]{opciones*}[\opcion](1)

\NewTasksEnvironment[label={$\ast$},
label-width=0.5em,item-indent=1em,item-format={\raggedright},%
label-offset=0.5em,before-skip=0pt,after-skip=0pt]{lista}[*](1)

\newtcolorbox{caja}[1][]{enhanced,colback=white,colframe=black,boxrule=1pt,attach boxed title to top left={yshift=-\tcboxedtitleheight/2,xshift=10pt},
boxed title style={colback=white},coltitle=black,height=30pt,#1}

\DeclareTotalTColorBox{\doscolumnas}{ O{0.7} +m +m }{
  sidebyside,sidebyside adapt=right,blankest}%
  {#2\tcblower #3}

% We define a command that does nothing, just acts as a marker.
% This is what we will split the environment's content on.
\newcommand{\siguiente}{}

\ExplSyntaxOn % Activate LaTeX3 programming syntax

% --- Variable Declarations ---
% A sequence to hold the parts of the body after splitting.
\seq_new:N \l_myenv_body_parts_seq
% A token list to hold the first part of the content.
\tl_new:N \l_myenv_part_one_tl
% A token list to hold the second part of the content.
\tl_new:N \l_myenv_part_two_tl

% --- Internal Processing Function ---
% This function takes the body of the environment (#1) and splits it.
\cs_new_protected:Npn \myenv_process_body:n #1
  {
    % Split the input (#1) by the \siguiente command and store the parts in our sequence.
    \seq_set_split:Nnn \l_myenv_body_parts_seq { \siguiente } { #1 }

    % Take the first item from the sequence and store it in \l_myenv_part_one_tl
    \seq_pop_left:NN \l_myenv_body_parts_seq \l_myenv_part_one_tl

    % Take all remaining items in the sequence, join them together,
    % and store the result in \l_myenv_part_two_tl.
    % This correctly handles cases with no \siguiente or multiple \siguiente's.
    \tl_set:Nx \l_myenv_part_two_tl { \seq_use:Nn \l_myenv_body_parts_seq { } }
  }

% --- The Environment Definition ---
\NewDocumentEnvironment{columnas}{ O{0.7} O{c} +b }
  {
    % 1. Process the entire body (#1) of the environment using our helper function.
    \myenv_process_body:n { #3 }
  
    % 2. Now you can use the processed parts.
    % We use \tl_use:N to output the content stored in our token list variables.
    % For this example, we'll format them directly.
    %% helpers
    \tl_clear_new:N \l__columnas_case_tl
    \str_case:nnF { #2 }
    {
      { t } { \tl_set:Nn \l__columnas_case_tl { top~seam } }
      { c } { \tl_set:Nn \l__columnas_case_tl { center~seam } }
      { b } { \tl_set:Nn \l__columnas_case_tl { bottom~seam } }
    }
    % The default case if no match is found.
    { \tl_set:Nn \l__columnas_case_tl { center~seam } }
    
    %%
    \begin{tcolorbox}[sidebyside,sidebyside~adapt=right,blankest,lefthand~ratio=#1,
      fontupper=\raggedright,parbox=false,sidebyside~align={\tl_use:N \l__columnas_case_tl},
      before~upper={\bgroup
        \setlength{\abovedisplayskip}{0pt}
        \setlength{\belowdisplayskip}{0pt}
        \setlength{\abovedisplayshortskip}{0pt}
        \setlength{\belowdisplayshortskip}{0pt}
      },after~upper={\egroup},
      before~lower={\bgroup
        \setlength{\abovedisplayskip}{0pt}
        \setlength{\belowdisplayskip}{0pt}
        \setlength{\abovedisplayshortskip}{0pt}
        \setlength{\belowdisplayshortskip}{0pt}
      },after~lower={\egroup}
      ]
    \tl_use:N \l_myenv_part_one_tl
    \tcblower
    \tl_use:N \l_myenv_part_two_tl
    \end{tcolorbox}
  }
  {
    % The "end" part of the environment is empty in this case.
  }

\ExplSyntaxOff % Deactivate LaTeX3 syntax

\NewDocumentEnvironment{pauta}{O{}+b}{%
  \begin{singlespace}
  \begin{tblr}{width=176mm,colspec={X[6]X[1,c]X[1,c]}, hline{2,Z} = {1}{-}{}, hline{2,Z} = {2}{-}{},
    hlines, cells={valign=m}, row{2} = {bg=black!15},vlines, cell{3-Z}{1}={cmd=\raggedright},#1}
    \SetCell[c=3]{c} \stop No rayar la tabla, es solo para uso del docente \stop &  &\\
    \SetCell{c} Criterios para la corrección & Puntaje & Asignación \\
    #2
}{%
  \end{tblr}\end{singlespace}
}

%%% TERMINA PARTE EXPERIMENTAL

\AtBeginDocument{%
  \pagenumbering{arabic}%
	\hypersetup{%
		%pdftitle = {\srstitulostr - \srssubtitulostr},%
    %pdfauthor = {\srsnombrestr},%
    pdfcreator = {Vscode + LuaTex}%
	}%
  \ifborradorTF{}%
  {%
  \begin{center}%
    {\sffamily\Large\bfseries \srstitulo} - {\sffamily\Large\srssubtitulo}%
  \end{center}%
  }%
  \ifconnombreTF{%
  \begin{tcbraster}[enhanced,raster columns=3,raster width=\linewidth,%
    raster column skip=3pt,raster force size=false,raster after skip=15pt,%
    raster before skip=10pt]%
    \begin{caja}[title={\sffamily\bfseries Nombre},height=35pt,add to width=4cm]%
    \end{caja}%
    \begin{caja}[title={\sffamily\bfseries Puntaje},height=35pt,add to width=-2cm,valign=bottom,halign=center]%
      \ifpuntajeTF{\hspace*{30pt}/ \srspuntaje}{}%
    \end{caja}%
    \begin{caja}[title={\sffamily\bfseries Nota},height=35pt,add to width=-2cm]%
    \end{caja}%
  \end{tcbraster}%
  \hfill\textbf{\sffamily Fecha:} \srsfecha\hspace*{10pt}\cancelarsalto%
  }{}
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

\newcommand{\q}[1]{``#1''}


\NewDocumentEnvironment{doteado}{b}{%
\newline\noindent\hspace*{-4em}\begin{minipage}{\textwidth}
  \begin{tcolorbox}[enhanced,
    borderline={1pt}{0pt}{black,dashed},colframe=white,colback=white,center,
    before skip=-2pt,after skip=3pt,hbox]
      #1
  \end{tcolorbox}
\end{minipage}
}{}


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

\def\stop{{\FA\char"E204}\space}
