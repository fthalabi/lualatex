\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{presentacion}[2024/11/04 Minimal Beamer-based class]

\PassOptionsToClass{noamsthm,professionalfonts,10pt}{beamer}
\LoadClass{beamer}

\providecommand{\colegio}{Colegio}
\providecommand{\asignatura}{Departamento de Matemática}
\providecommand{\titulo}{Titulo}
\providecommand{\subtitulo}{Subtitulo}
\providecommand{\nombre}{Prof. Fernando Halabi}
\providecommand{\correo}{correo@ASDAS.cl}

\makeatletter
\newcommand{\disablepackage}[2]{%
  \disable@package@load{#1}{#2}%
}
\newcommand{\reenablepackage}[1]{%
  \reenable@package@load{#1}%
}
\makeatother

\disablepackage{titlesec}{}
\disablepackage{enumitem}{}
\disablepackage{lastpage}{}
\disablepackage{fancyhdr}{}
\input{paquetes}

% Define variables for options
\newboolean{modoclaro}
\newboolean{optiontwo}
\newboolean{optionthree}
% Set default values
\setboolean{modoclaro}{false}
\setboolean{optiontwo}{false}
\setboolean{optionthree}{false}
% Declare options
\DeclareOption{temaclaro}{\setboolean{modoclaro}{true}}
\DeclareOption{sinnombre}{\setboolean{optiontwo}{true}}
\DeclareOption{conautor}{\setboolean{optionthree}{true}}
% Handle unknown options
\DeclareOption*{\PackageWarning{yourclassname}{Unknown option '\CurrentOption'}}
% Process the options
\ProcessOptions\relax

\usefonttheme{professionalfonts}
\renewcommand{\familydefault}{\rmdefault}


\geometry{
  paperwidth=18cm,
  paperheight=13cm,
  left=1cm,
  right=1cm
}

% Polar Night
\definecolor{NordDarkBlack}{HTML}{2E3440}     % nord0
\definecolor{NordBlack}{HTML}{3B4252}         % nord1
\definecolor{NordMediumBlack}{HTML}{434C5e}   % nord2
\definecolor{NordBrightBlack}{HTML}{4C566A}   % nord3
% Snow Storm
\definecolor{NordWhite}{HTML}{D8DEE9}         % nord4
\definecolor{NordBrighterWhite}{HTML}{E5E9F0}         % nord5
\definecolor{NordBrightestWhite}{HTML}{ECEFF4}   % nord6
% Frost
\definecolor{NordCyan}{HTML}{8FBCBB}          % nord7
\definecolor{NordBrightCyan}{HTML}{88C0D0}    % nord8
\definecolor{NordBlue}{HTML}{81A1C1}          % nord9
\definecolor{NordBrightBlue}{HTML}{5E81AC}    % nord10
% Aurora
\definecolor{NordRed}{HTML}{BF616A}           % nord11
\definecolor{NordOrange}{HTML}{D08770}        % nord12
\definecolor{NordYellow}{HTML}{EBCB8B}        % nord13
\definecolor{NordGreen}{HTML}{A3BE8C}         % nord14
\definecolor{NordMagenta}{HTML}{B48EAD}       % nord15


\definecolor{MonokaiBrown}{HTML}{2C2525}       % nord15
\definecolor{MonokaiBlack}{HTML}{211F22}       % nord15
\definecolor{MonokaiRed}{HTML}{FF6188}       % nord15
\definecolor{MonokaiOrange}{HTML}{FC9867}       % nord15
\definecolor{MonokaiYellow}{HTML}{FFD866}       % nord15
\definecolor{MonokaiGreen}{HTML}{A8DC76}       % nord15
\definecolor{MonokaiBlue}{HTML}{78DCE8}       % nord15
\definecolor{MonokaiPurple}{HTML}{AC9CF2}       % nord15
\definecolor{MonokaiWhite}{HTML}{FCFCFA}       % nord15


\colorlet{ColorAlertedText}{NordRed}
\colorlet{ColorSubtitle}{NordBlue}
\colorlet{ColorSubsectionInToc}{ColorSubtitle}

\ifthenelse{\boolean{modoclaro}}{
  \colorlet{ColorBackground}{white}
  \colorlet{ColorText}{black}
  \colorlet{ColorExampleText}{NordCyan}
  \colorlet{ColorStructure}{NordBrightBlue}
  \colorlet{ColorTitle}{NordBrightBlue}
  \colorlet{ColorSubtitle}{NordBlue}
  \colorlet{ColorSubsectionInTocShaded}{ColorSubsectionInToc!50!ColorBackground}
  \colorlet{ColorItemizeItem}{ColorStructure}
  \colorlet{ColorBlockTitleBg}{NordGreen!50}
  \colorlet{ColorExampleBlockTitleBg}{ColorExampleText!50}
  \colorlet{ColorAlertedBlockTitleBg}{ColorAlertedText!50}
  \colorlet{ColorBlockBodyBg}{NordGreen!20}
  \colorlet{ColorExampleBlockBodyBg}{ColorExampleText!20}
  \colorlet{ColorAlertedBlockBodyBg}{ColorAlertedText!20}
  \colorlet{ColorBlockBodyFg}{ColorText}
  \colorlet{ColorExampleBlockBodyFg}{ColorText}
  \colorlet{ColorAlertedBlockBodyFg}{ColorText}
  \colorlet{ColorPrevio}{NordGreen!50}
  \colorlet{ColorActual}{NordBrightCyan}
  \colorlet{ColorSiguiente}{NordBrightBlack!30}
}{
  \colorlet{ColorBackground}{MonokaiBlack}
  \colorlet{ColorText}{MonokaiWhite!70!MonokaiBlack}
  \colorlet{ColorExampleText}{NordBrightCyan}
  \colorlet{ColorStructure}{NordCyan}
  \colorlet{ColorTitle}{NordBlue}
  \colorlet{ColorSubtitle}{NordBlue!80!MonokaiBlack}
  \colorlet{ColorSubsectionInTocShaded}{NordCyan!20!NordBrightBlack}
  \colorlet{ColorItemizeItem}{NordBlue}
  \colorlet{ColorBlockTitleBg}{NordBlack!80}
  \colorlet{ColorExampleBlockTitleBg}{ColorBlockTitleBg}
  \colorlet{ColorAlertedBlockTitleBg}{ColorBlockTitleBg}
  \colorlet{ColorBlockBodyBg}{NordBlue!50!NordBlack}
  \colorlet{ColorExampleBlockBodyBg}{NordBrightCyan!50!NordBlack}
  \colorlet{ColorAlertedBlockBodyBg}{NordRed!50!NordBlack}
  \colorlet{ColorBlockBodyFg}{NordCyan}
  \colorlet{ColorExampleBlockBodyFg}{NordWhite}
  \colorlet{ColorAlertedBlockBodyFg}{NordWhite}
  \colorlet{ColorPrevio}{NordBlue!50!MonokaiBlack}
  \colorlet{ColorActual}{NordBrightCyan}
  \colorlet{ColorSiguiente}{NordBrightBlack!30!MonokaiBlack}
}


\colorlet{ColorNormalText}{ColorText}
\colorlet{ColorFrameTitle}{ColorTitle}
\colorlet{ColorFrameSubtitle}{ColorSubtitle}
\colorlet{ColorSectionInToc}{ColorTitle}
\colorlet{ColorEnumerateColor}{ColorItemizeItem}
\colorlet{ColorSectionInHeadFoot}{ColorStructure}

%%%%% set beamer color
\setbeamercolor*{background canvas}{bg=ColorBackground}
\setbeamercolor*{normal text}{fg=ColorNormalText}
\setbeamercolor*{alerted text}{fg=ColorAlertedText}
\setbeamercolor*{example text}{fg=ColorExampleText}
\setbeamercolor*{structure}{fg=ColorStructure}

\setbeamercolor*{title}{fg=ColorTitle}
\setbeamercolor*{subtitle}{fg=ColorSubtitle}
\setbeamercolor*{frametitle}{fg=ColorFrameTitle}
\setbeamercolor*{framesubtitle}{fg=ColorFrameSubtitle}
\setbeamercolor*{section in toc}{fg=ColorSectionInToc}
\setbeamercolor*{subsection in toc}{fg=ColorSubsectionInToc}
\setbeamercolor*{subsection in toc shaded}{fg=ColorSubsectionInTocShaded}

\setbeamercolor*{itemize item}{fg=ColorItemizeItem}
\setbeamercolor*{enumerate item}{fg=ColorEnumerateColor}

\setbeamercolor*{section in head/foot}{bg=ColorBackground, fg=ColorSectionInHeadFoot}

\setbeamercolor{block title}{bg=ColorBlockTitleBg,fg=NordGreen}
\setbeamercolor{block title example}{bg=ColorExampleBlockTitleBg,parent={example text}}
\setbeamercolor{block title alerted}{bg=ColorAlertedBlockTitleBg,parent={alerted text}}
\setbeamercolor{block body}{bg=ColorBlockBodyBg,fg=ColorBlockBodyFg}
\setbeamercolor{block body example}{bg=ColorExampleBlockBodyBg,fg=ColorExampleBlockBodyFg}
\setbeamercolor{block body alerted}{bg=ColorAlertedBlockBodyBg,fg=ColorAlertedBlockBodyFg}

\setbeamercolor{caption}{fg=NordBrightBlue}

%%%%%%%%%%%%%%%%

\setbeamertemplate{navigation symbols}{}
\setbeamertemplate{caption}{\insertcaption}

\setbeamertemplate{itemize item}{\tikz[baseline=-.6ex]{\fill (0,0) circle (2pt);}}
\setbeamertemplate{itemize subitem}{\tikz[baseline=-.6ex]{\draw (0,0) circle (2pt);}}
\setbeamertemplate{frametitle}{
  {\sffamily\bfseries\LARGE\insertframetitle}%
  \hfill\ifx\insertframesubtitle\@empty\else{\sffamily\large\usebeamercolor[fg]{framesubtitle}\insertframesubtitle}\fi
}
\setbeamertemplate{frametitle continuation}[from second][\insertcontinuationtext]
\setbeamertemplate{headline}
{%
\begin{beamercolorbox}{section in head/foot}
\vskip2pt\insertnavigation{\paperwidth}\vskip5pt
\end{beamercolorbox}%
}

\setbeamertemplate{footline}{
  \ifnum \insertframenumber=1
  \else
  {\centering
  \begin{tikzpicture}
    \foreach \i in {1,...,\inserttotalframenumber} {
      \ifnum\i<\insertframenumber
        \node [rounded rectangle, minimum width=15pt, minimum height=5pt,fill=ColorPrevio] at (18pt*\i,0) {};
      \else
        \ifnum\i=\insertframenumber
          \node [rounded rectangle, minimum width=15pt, minimum height=5pt,fill=ColorActual] at (18pt*\i,0) {};
        \else
          \node [rounded rectangle, minimum width=15pt, minimum height=5pt,fill=ColorSiguiente] at (18pt*\i,0) {};
        \fi
      \fi
    }
  \end{tikzpicture}\par\vspace{5pt}}
  \fi
}

\AtBeginDocument{%
\begin{frame}{}{}
  {\usebeamercolor[fg]{title}\bfseries\sffamily\Huge\titulo}\\
  {\usebeamercolor[fg]{subtitle}\bfseries\sffamily\LARGE\subtitulo\\[70pt]}
  {\usebeamercolor[fg]{normal text}\nombre\\[5pt]}
  {\usebeamercolor[fg]{normal text}\colegio\\}

\end{frame}
}

\pgfplotsset{
  eje escolar/.append style={
    tick label style={
      fill=ColorBackground,
    },
  }
}