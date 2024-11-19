\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{experimento}[2024/10/21 v1.0 Plantilla para hackear en latex]

\PassOptionsToClass{10pt,spanish,oneside,letterpaper}{article}
\LoadClass{article}

\input{paquetes}

\pgfkeys{/pgf/number format/.cd,
  fixed,
  precision=2,
  set thousands separator={.},
  use comma
}

\geometry{
  paper=letterpaper,
  top=2cm,
  bottom=2cm,
  left=1.5cm,
  right=1.5cm,
  headheight=2cm,
  headsep=15pt,
  footskip=20pt,
  %showframe
}

\pagestyle{empty}

\AtBeginDocument{%
	\hypersetup{%
		pdftitle = Experimento,
    pdfauthor = Fernando H.,
    pdfcreator = Vscode + LuaTex
	}%
  \raggedright
}