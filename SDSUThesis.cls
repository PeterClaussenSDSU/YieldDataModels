%%%%%%%%%%%%%%%%%%%%%%%
%% SDSU Thesis Class %%
%%%%%%%%%%%%%%%%%%%%%%%
\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{SDSUThesis}[03/03/2009 version 1.0]
\RequirePackage{xkeyval} %allows key=value pairs in class options.
% Revision History: 
% Created by Erin Nichols, 2009--2010.
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% SDSUThesis Class Options & related macros %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% What type of document will this be?
% By default, it will be a Master's thesis in math, so we make the macros.
\def\@Doctype{Thesis}       % Doctype must be capitalized.
\def\@degree{Master of Science}
\def\@major{Mathematics}
\DeclareOption{msmath}{}    % dummy option, macros above already set.
%% Master's thesis in stats %%
\DeclareOption{msstat}{%    redefine major; Doctype & degree correct.
  \def\@major{Statistics}
}%
%% PhD Dissertation in CSS %%
\DeclareOption{phdcss}{% redefine all macros.
  \def\@degree{Doctor of Philosophy}
  \def\@Doctype{Dissertation}
  \def\@major{Computational Science and Statistics}
}
% we need a capitalized and lowercase version of doctype.
\def\@doctype{\MakeLowercase{\@Doctype}}
% pass hyperref options to ensure chapters are converted to
% PDF bookmarks passing it to geometry actually makes sure
% you convert to the correct paper size too!
\DeclareOption{pdftex}{%
    \PassOptionsToPackage{pdftex}{hyperref}
    \PassOptionsToPackage{pdftex}{geometry}}
\DeclareOption{dvipdfm}{%
    \PassOptionsToPackage{dvipdfm}{hyperref}
    \PassOptionsToPackage{dvipdfm}{geometry}}
\DeclareOption{dvips}{%
    \PassOptionsToPackage{dvips}{hyperref}
    \PassOptionsToPackage{dvips}{geometry}}
\DeclareOption{raggedbottom}{\raggedbottom}
\DeclareOption{flushbottom}{\flushbottom}
%
%% Required by grad school:
%
% "type should be 10 or 12 point."
%
\PassOptionsToClass{12pt}{book}
%
% "1. The text must be on one side of the page"
%
\PassOptionsToClass{oneside}{book}
%
% "11. The original ... must measure 8.5 X 11 inches."
%
\PassOptionsToClass{letterpaper}{book}
\PassOptionsToClass{letterpaper}{geometry}
%
% Margin adjustments
%
\newdimen\@leftmar
\setlength{\@leftmar}{1.36in}
\newdimen\@rightmar
\setlength{\@rightmar}{.94in}
\newdimen\@topmar
\setlength{\@topmar}{1.25in}
\newdimen\@botmar
\setlength{\@botmar}{1in}
\newdimen\@headmar
\setlength{\@headmar}{24pt}
\DeclareOptionX{leftadj}{\addtolength{\@leftmar}{#1}}
\DeclareOptionX{rightadj}{\addtolength{\@rightmar}{#1}}
\DeclareOptionX{topadj}{\addtolength{\@topmar}{#1}}
\DeclareOptionX{botadj}{\addtolength{\@botmar}{#1}}
\DeclareOptionX{headadj}{\addtolength{\@headmar}{#1}}
%
% highfalutin' Draft and Confidential options
%
\newif\ifisconfidential
\isconfidentialfalse
\newif\ifisadraft
\isadraftfalse
\DeclareOption{draft}{%
\isadrafttrue
\PassOptionsToClass{draft}{book}
}
\DeclareOption{confidential}{%
\isconfidentialtrue
}
%
% pass unknown options to book class, process options, load book class
\DeclareOption*{\PassOptionsToClass{\CurrentOption}{book}}
\ExecuteOptions{dvips,letterpaper,raggedbottom}
\ProcessOptionsX*
\ProcessOptions
\LoadClass{book}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Some Formatting Things %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% "Margins at the top and left must be 1.5 inches. Margins at the bottom and
% right of page must be 1 inch."
%
% "Pages are to be numbered 1 inch from the top right-hand corner and 1 inch
% from the right side of the paper."
%
\usepackage{geometry}
%\geometry{left=1.36in, top=1.25in, right=.94in, bottom=1in, headheight=15pt, headsep=24pt, nofoot}% options appended.
\geometry{left=\@leftmar, top=\@topmar, right=\@rightmar, bottom=\@botmar, 
    headheight=15pt, headsep=\@headmar, nofoot}% options appended.
\usepackage{fancyhdr}   % better control over page headers.
\pagestyle{fancy}
\fancyhead{}
\fancyfoot{}
\fancyhead[R]{\thepage}
\renewcommand{\headrulewidth}{0pt}
%
% "1. The text must be on one side of the page and double-spaced."
%
\usepackage{setspace}
%\doublespacing does not actually double space. That does \setstretch{1.66}
\setstretch{2.0}
%
%  "4. Margins must be left justified only."
%
\raggedright
%
%  "5. Paragraphs must be indented 0.5 inches"
%
\usepackage{indentfirst}
\setlength{\parindent}{0.5in}
%
% Samples from the grad office use plain text for chapter, section titles:
\usepackage{tocloft} % customizes table of contents, list of figures/tables
\usepackage{sectsty} % customizes section styles (in document, not ToC)
\renewcommand{\cfttoctitlefont}{\hfill}
\renewcommand{\cftaftertoctitle}{\hfill}
\renewcommand{\cftloftitlefont}{\hfill}
\renewcommand{\cftafterloftitle}{\hfill}
\renewcommand{\cftlottitlefont}{\hfill}
\renewcommand{\cftafterlottitle}{\hfill}
\renewcommand{\cftpartfont}{}
\renewcommand{\cftpartpagefont}{}
\renewcommand{\cftchapfont}{}
\renewcommand{\cftchappagefont}{}
\renewcommand{\cftsecfont}{}
\renewcommand{\cftsecpagefont}{}
\renewcommand{\cftchapdotsep}{\cftdotsep}
\renewcommand{\cftpartdotsep}{\cftdotsep}
\renewcommand{\cftbeforetoctitleskip}{-2.5em}
\renewcommand{\cftbeforelottitleskip}{-2.5em}
\renewcommand{\cftbeforeloftitleskip}{-2.5em}
\renewcommand{\cftaftertoctitleskip}{0em}
\renewcommand{\cftafterlottitleskip}{0em}
\renewcommand{\cftafterloftitleskip}{0em}
\allsectionsfont{\usefont{OT1}{cmr}{m}{n}\normalsize\selectfont}
%
% define variables for the maketitle.
%
\def\@author{}
\def\@title{}
\def\@docmonth{ \ifcase\month\or
January\or February\or March\or April\or May\or June\or
July\or August\or September\or October\or November\or December\fi
}
\def\@docyear{\number\year}
\def\@docadvisor{}
\def\@depthead{}
\def\@dept{}
\def\@thirdsig{}
\def\@thirdsigtitle{}
\def\@siggap{1in} % adjust vertical space below each signature line.
\def\@keywords{\@author , \@title}
\newcommand{\docmonth}[1]{\def\@docmonth{#1}}
\newcommand{\docyear}[1]{\def\@docyear{#1}}
\renewcommand{\author}[1]{\def\@author{#1}}
\newcommand{\docadvisor}[1]{\def\@docadvisor{#1}}
\newcommand{\depthead}[1]{\def\@depthead{#1}}
\newcommand{\dept}[1]{\def\@dept{#1}}
\newcommand{\thirdsig}[1]{\def\@thirdsig{#1}}
\newcommand{\thirdsigtitle}[1]{\def\@thirdsigtitle{#1}}
\newcommand{\siggap}[1]{\def\@siggap{#1}}
\newcommand{\keywords}[1]{\def\@keywords{#1}}%
% make sure to include pdftex in all relevent packages
% as it will automatically put bookmarks in pdf files generated
%%%%%%%%%%%%%%%%%%%%%%%
%% Fix Chapter Heads %%
%%%%%%%%%%%%%%%%%%%%%%%
% Chapter heads must be at top of page. This was copied from book.cls
% then edited to remove \vskip commands.
% \makechapterhead is for \chapter, \makeschapterhead does \chapter*.
\renewcommand{\@makechapterhead}[1]{%
  {\parindent \z@ \raggedright \normalfont \centering
    \ifnum \c@secnumdepth >\m@ne
      \if@mainmatter
        \bfseries \@chapapp\space \thechapter
        \par\nobreak
      \fi
    \fi
    \interlinepenalty\@M
    \bfseries #1\par\nobreak
  }}
\renewcommand\@makeschapterhead[1]{%
  {\parindent \z@ \raggedright \centering
    \normalfont
    \interlinepenalty\@M
    \bfseries  #1\par\nobreak
  }}
% This should stop new chapters changing pagestyle
\renewcommand{\chapter}{%
    \clearpage
    \secdef\@chapter\@schapter
}%
% Hyperref is awexome.
\RequirePackage[unicode, bookmarksnumbered, plainpages=false, pdfcreator={SDSUThesis}, pdftex, ]{hyperref}
%%%%%%%%%%%%%%%%
%% Title Page %%
%%%%%%%%%%%%%%%%
% We'll override maketitle.
% "The first line is placed 1.5 inches from the top margin and is centered
%  between typing margins. The author’s name is centered between margins on
%  about the 28th line from the top of the page (4.5 inches). The year the
%  degree is conferred is centered 1 inch from the bottom margin and the
%  submission statement is typed above it."
% However, the grad school's own example has the title right under the top 
% margin, 1.5 inches from the top of the _page_.
\renewcommand{\maketitle}{%
\hypersetup{    pdfauthor=\@author,    pdftitle=\@title,    pdfkeywords=\@keywords, }
\pdfbookmark[1]{Title Page}{titlepage}
\begin{titlepage}%
    \begin{center}%
    \@title\\
    \vspace{2.75in}
    BY\\
    \@author

    \vspace\fill
    A {\@doctype} submitted in partial fulfillment of the requirements for the\\
    {\@degree} \\
    Major in \@major \\
    South Dakota State University\\
    \@docyear 
    \end{center}%
\end{titlepage}
\setcounter{page}{2}
}
%%%%%%%%%%%%%%%%%%%%%
%% Acceptance Page %%
%%%%%%%%%%%%%%%%%%%%%
% "The first line is placed 1.5 inches from the top margin and contains the 
%  title of the paper. The statement of acceptance and names of those 
%  approving the thesis/dissertation are typed according to the example.
%  NOTE: When the thesis/dissertation advisor and the major advisor are not the 
%  same, both names should be included on the acceptance page."
% Again, the grad school seems to have confused margin with page.
%
% 1. Signature Block.
\newcommand{\signatureline}[2]{%
    \singlespace
    \begin{tabular*}{3.5in}{@{\extracolsep{\fill}} l r}
    \hline
    {#1} \\
    {#2} & Date\\
    \end{tabular*}
}%
% 2. The actual acceptance page.
\newcommand{\acceptancepage}{%
    \clearpage
    \pdfbookmark[1]{Acceptance Page}{signatures}
    \begin{center}
    \@title\\
~\\
    \end{center}
    \par This {\@doctype} is approved as a cretitable and independent investigation by a candidate for the {\@degree} degree and is acceptable for meeting the {\@doctype} requirements for this degree. Acceptance of this {\@doctype} does not imply that the conclusions reached by the candidate are necessarily the conclusions of the major department.
    \vspace*\fill

    \begin{flushright}
    \signatureline{\@docadvisor}{\@Doctype\ Advisor} \\
    \vspace{\@siggap}
    \ifx \@thirdsig \@empty
        %do nothing
    \else
        \signatureline{\@thirdsig}{\@thirdsigtitle} \\
        \vspace{\@siggap}
    \fi
    \signatureline{\@depthead}{Head, \@dept}
    \end{flushright}
    \vspace{\@siggap}
    \clearpage
}
%%%%%%%%%%%%%%
%% Abstract %%
%%%%%%%%%%%%%%
% The book document class does not define abstract environment.
% With other classes, we would call renewenvironment
\newenvironment{abstract}{%
\chapter{Abstract}
\begin{center}\vspace{2em}
\@title\\
\@author\\
\@docmonth~\@docyear\\
\end{center}
}{\clearpage
}
%%%%%%%%%%%%%%%%%%%%%%
%% Acknowledgements %%
%%%%%%%%%%%%%%%%%%%%%%
\newenvironment{acknowledgments}{%
\chapter{Acknowledgements}
}{\clearpage
}%

\tocloftpagestyle{fancy} % set toc page style to fancy like rest of doc

% the highfalutin' draft and confidential stuff for the header.
\ifx\ifisconfidential\ifisadraft
    \ifisadraft
        \fancyhead[L]{\textsc{Draft---\today{}---Confidential}}
    \fi
\else
    \ifisadraft
        \fancyhead[L]{\textsc{Draft---\today{}}}
    \else
        \fancyhead[L]{\textsc{Confidential}}
    \fi
\fi
