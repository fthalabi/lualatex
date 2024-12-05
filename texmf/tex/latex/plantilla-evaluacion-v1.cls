\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{plantilla-evaluacion-v1}[2024/9/28 v1.0 Config para generar evaluación]

\PassOptionsToClass{10pt,oneside,letterpaper}{article}
\LoadClass{article}

\providecommand{\colegio}{COLEGIO}
\providecommand{\asignatura}{Departamento de Matemática}
\providecommand{\titulo}{TITULO}
\providecommand{\subtitulo}{SUBTITULO}
\providecommand{\nombre}{Prof. Fernando Halabi}
\providecommand{\correo}{correo@ASDAS.cl}

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
\DeclareOption{sincurso}{\setboolean{optionone}{true}}
\DeclareOption{sinnombre}{\setboolean{optiontwo}{true}}
\DeclareOption{conautor}{\setboolean{optionthree}{true}}
% Handle unknown options
\DeclareOption*{\PackageWarning{yourclassname}{Unknown option '\CurrentOption'}}
% Process the options
\ProcessOptions\relax

\renewcommand{\thesection}{\Roman{section}}
\titleformat{\section}{\sffamily\bfseries\Large}{\thesection.}{1.5em}{}

\geometry{
  paper=letterpaper,
  top=2cm,
  bottom=2cm,
  left=2cm,
  right=2cm,
  headheight=2cm,
  headsep=15pt,
  footskip=20pt,
  marginparwidth=0pt,
  includemp=false,
  heightrounded,
  %showframe
}
\setlength{\marginparwidth}{0pt}
\setlength{\marginparsep}{0pt}

\pagestyle{fancy}
\fancyhfoffset[H]{0pt} 
\fancyhead[C]{\ifthenelse{\thepage=1}{}{\sffamily{\scshape\bfseries\titulo} - \subtitulo}}
\fancyhead[L]{\ifthenelse{\thepage=1}{%
  \footnotesize{\colegio\\{\asignatura}\ifthenelse{\isundefined{\curso}}{}{{ -} {\curso}} }
  }{}
}
\fancyhead[R]{\ifthenelse{\boolean{optionthree} \AND \thepage=1}{\footnotesize\nombre}{}}
\fancyfoot[L,C]{}
\fancyfoot[R]{\footnotesize{Página {\thepage} de {\pageref*{LastPage}}}}

%\renewcommand{\thesection}{\Roman{section}}
%\titleformat{\section}{\sffamily\scshape\bfseries\large}{\thesection.}{1.5em}{}


\newtcolorbox{caja}[1][]{enhanced,colback=white,colframe=black,boxrule=1pt,attach boxed title to top left={yshift=-\tcboxedtitleheight/2,xshift=10pt},
boxed title style={colback=white},coltitle=black,height=30pt,#1}

\newtcolorbox{malla}[1][]{enhanced,inherit height,colback=white,colframe=black,boxrule=1pt,underlay={\begin{tcbclipinterior}
  \draw[help lines,step=5mm,yshift=0pt] (interior.south west) grid (interior.north east);\end{tcbclipinterior}},#1}

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
   on line, boxsep=0pt, left=#1, right=#1, top=#2, bottom=#2,%
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

\newlength{\labelwith}
\setlength{\labelwidth}{\widthof{\mybox[1pt][2pt]{\widthof{22}}{\sffamily\bfseries 12}}}
%\NewTasksEnvironment[label={\mybox[1pt][2pt]{\widthof{M}}{\sffamily\bfseries\scshape\alph*}},
%label-width=\labelwidth,item-indent=30pt,
%label-offset=10pt,column-sep=10pt]{ejercicios}[\ejercicio](2)

\NewTasksEnvironment[label={\mybox[1pt][2pt]{\widthof{22}}{\sffamily\bfseries\scshape\arabic*}},
label-width=\labelwidth,item-indent=30pt,resume=true,
label-offset=10pt,column-sep=10pt]{preguntas}[\pregunta](1)


%% Setting values
\newlength{\myitemsep}
\setlength{\myitemsep}{3pt}
\newlength{\mytopsep}
\setlength{\mytopsep}{5pt}

\NewTasksEnvironment[
  label={{\itshape\alph*})},
label-width=10pt,item-indent=20pt,label-offset=10pt,after-item-skip=\myitemsep,
before-skip=\mytopsep,after-skip=\mytopsep
]{alternativas}[\alternativa](1)

\NewDocumentEnvironment{vertical}{O{}+b}%
{%
    \newcommand{\alternativa}{\item}
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

\NewDocumentEnvironment{horizontal}{O{}+b}%
{%
    \newcommand{\alternativa}{\item}
    \begin{enumerate*}[label={{\itshape\alph*})},itemjoin={\hspace{10pt} },#1]
        #2
}{%
    \end{enumerate*}
}

%\NewDocumentEnvironment{preguntas}{O{}+b}%
%{%
%    \newcommand{\pregunta}{\item}
%    \begin{minipage}{\linewidth}
%      \vspace*{\mytopsep}
%      \begin{enumerate}[label={{\itshape\alph*})},
%        labelsep=10pt,leftmargin=0pt,labelwidth={\widthof{{\itshape a})}},
%        itemindent={\widthof{{\itshape a})}}+10pt,labelindent=0pt,itemsep=\myitemsep,topsep=\mytopsep,
%        listparindent=0pt,#1]
%          #2      
%}{%
%    \end{enumerate}
%  \end{minipage}
%}


\NewDocumentCommand{\respuestas}{}{%
\begin{minipage}[b][\lineskip+15pt][t]{\linewidth}
  \centering
  \sffamily\Large {\scshape\bfseries Respuestas}
\end{minipage}
}

\NewDocumentEnvironment{alternativasgraficas}{O{}+b}{%
  \NewDocumentCommand{\alternativa}{O{}}{\tcbitem[##1]}
  \begin{tcbitemize}[raster columns=2,raster equal height=rows,blank,lefttitle=0pt,
    leftupper=10pt,fonttitle=\itshape,valign=center,halign=center,
    coltitle=black,title={\alphalph{\thetcbrasternum})},#1]
      #2
}{%
  \end{tcbitemize}
}

\AtBeginDocument{%
	\hypersetup{%
		pdftitle = {\titulo - \subtitulo},
    pdfauthor = {\nombre},
    pdfcreator = {Vscode + LuaTex}
	}%
  %\begin{minipage}[b][\lineskip+15pt][t]{\linewidth} 
  %  \centering
  %  \sffamily\Large {\scshape\bfseries \titulo} - \subtitulo
  %\end{minipage}
  \begin{center}
    {\sffamily\Large\scshape\bfseries \titulo} - {\sffamily\Large\subtitulo}
  \end{center}
  %\datos
  %\raggedright%
  \ifthenelse{\boolean{optiontwo}}{}{%
    \ifthenelse{\boolean{optionone}}{%
      \begin{tcbraster}[enhanced,raster columns=3,raster width=\linewidth,
        raster column skip=3pt,raster force size=false,raster after skip=15pt,  
        raster before skip=10pt]
        \begin{caja}[title={\sffamily\scshape\bfseries Nombre},height=35pt,add to width=4cm]
        \end{caja}  
        \begin{caja}[title={\sffamily\scshape\bfseries Puntaje},height=35pt,add to width=-2cm,valign=bottom,halign=center]
          \ifthenelse{\isundefined{\puntaje}}{}{\hspace*{30pt}/ \puntaje}
        \end{caja}
        \begin{caja}[title={\sffamily\scshape\bfseries Nota},height=35pt,add to width=-2cm]
        \end{caja}      
      \end{tcbraster}%
    }{%
      \begin{tcbraster}[enhanced,raster columns=4,raster width=\linewidth,
        raster column skip=3pt,raster force size=false,raster after skip=15pt,
        raster before skip=10pt]%
        \begin{caja}[title={\sffamily\scshape\bfseries Nombre},height=35pt,add to width=4.5cm]
        \end{caja}
        \begin{caja}[title={\sffamily\scshape\bfseries Curso},height=35pt,add to width=-1.5cm]
        \end{caja}    
        \begin{caja}[title={\sffamily\scshape\bfseries Puntaje},height=35pt,add to width=-1.5cm,valign=bottom,halign=center]
          \ifthenelse{\isundefined{\puntaje}}{}{\hspace*{30pt}/ \puntaje}
        \end{caja}
        \begin{caja}[title={\sffamily\scshape\bfseries Nota},height=35pt,add to width=-1.5cm]
        \end{caja}      
      \end{tcbraster}%
    }
  }%
}

\NewDocumentCommand{\negrita}{m}{{\sffamily\scshape\bfseries\large#1}}
\NewDocumentCommand{\f}{m}{\pgfmathprintnumber[fixed,fixed zerofill,precision=3,verbatim,use comma]{#1}}
