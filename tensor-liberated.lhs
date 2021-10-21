%% -*- latex -*-

%% \documentclass[aspectratio=43]{beamer}
\documentclass[aspectratio=169]{beamer}

%% \setbeamercolor{background canvas}{bg=white} %% otherwise ``light''

\usecolortheme{crane}%whale,beaver
\usefonttheme{serif}
\useinnertheme[shadow]{rounded}
% \useoutertheme{default}
\useoutertheme{shadow}
% \useoutertheme{infolines}
% Suppress navigation arrows
\setbeamertemplate{navigation symbols}{}

\setbeamersize{text margin left=.5cm,text margin right=.5cm}

\usepackage{catchfilebetweentags}
\usepackage[useregional]{datetime2}

\RequirePackage{agda, tikz-cd, newunicodechar, amssymb, stmaryrd, unicode-math, setspace, comment, listings, anyfontsize}

\input{macros}
%% \input{commands}
\input{unicode}

%include polycode.fmt
%include forall.fmt
%include greek.fmt
%include formatting.fmt

%% Arrow labels are too small by default
\tikzcdset{every label/.append style = {font = \normalsize}}
\tikzcdset{every diagram/.append style = {ampersand replacement=\&}}

\tikzcdset{   row sep/normal=17ex}
\tikzcdset{column sep/normal=8em}

\nc\arD[2]{\arrow[#1, "#2"{description}]}
\nc\arR[1]{\arD{r}{#1} \&}
\nc\arU[1]{\arD{u}{#1}}
\nc\arUR[2]{\arD{u}{#1} \arD{r}{#2} \&}

%----------------------------------------------------------------------------

\title[Can Tensor Programming Be Liberated ...?]{Can Tensor Programming Be Liberated\\from the Fortran Data Paradigm?}
% \subtitle{...}
\author{Conal Elliott}
\date{October 2021}

\begin{document}

\maketitle

\begin{frame}{``This is the Unix philosophy:}
\begin{itemize}\itemsep4ex
\item Write programs that do one thing and do it well.
\item Write programs to work together.
\item Write programs to handle text streams, because that is a universal interface.''
\end{itemize}
\vspace{2ex}
\begin{flushright}
-- Doug McIlroy \hspace{0.5in}{\ }
\end{flushright}
\end{frame}

\begin{frame}{How Unix defeated its own philosophy}
%% \pause
\begin{itemize}\itemsep4ex
\item \textcolor{blue}{Write programs that do one thing and do it well.}
\item Write programs to work together.
\item \textcolor{red}{Write programs to handle text streams, because that is a universal interface.}
\end{itemize}

\pause
\vspace{5.5ex}
\emph{Every Unix program contains a parser (from text) and unparser (to text).}
\end{frame}

\begin{frame}{Likewise,}
\begin{center}
\emph{Every array program contains a parser (from arrays) and unparser (to arrays).}
\end{center}
\end{frame}

\begin{frame}{An efficient array program (CUDA C)}
\vspace{0.5ex}
\hspace{1in}\wpicture{4.35in}{cuda-and-beaker.pdf}
\end{frame}

\begin{frame}[fragile]{In NESL}
\vspace{4ex}
\begin{verbatim}
function scan(a) =
  if #a == 1 then [0]
  else
    let es = even_elts(a);
        os = odd_elts(a);
        ss = scan({e+o: e in es; o in os})
    in interleave(ss,{s+e: s in ss; e in es})
\end{verbatim}
\vspace{1ex}
{\scriptsize
\hspace{2.5in}\href{http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.53.5739}{Source: Guy Blelloch in \emph{Programming parallel algorithms, 1990}}}

\pause\vspace{2ex}
Still, why does it work?
\end{frame}

\begin{frame}{Realization}
\Large
\vspace{11ex}
\begin{center}
\emph{It's not naturally an array algorithm.}
\end{center}
\pause
\vspace{6ex}
What is it?
\pause
Hints:

\begin{itemize}
\item Size must be a power of two.
\item Work: $O (n)$, depth: $O (\log n)$.
\end{itemize}

\end{frame}

\rnc\SourceModule{Code}

%format totu = u"_{"tot"}"
%format totv = v"_{"tot"}"

\begin{frame}{Wrong guess}
\vspace{6.4ex}
\begin{code}
data T a = L a | B (T a) (T a)
\end{code}
\vspace{3in}
\end{frame}

\begin{frame}{Wrong guess}
\vfill
\begin{code}
data T a = L a | B (T a) (T a) deriving functor
\end{code}
\vfill
\begin{code}
scanT :: Monoid a => T a -> (T a , a)
scanT (L x)    = (L mempty , x)
scanT (B u v)  = (B u' (fmap (totu <> NOP) v') , totu <> totv)
  where
    (u', totu)  = scanT u
    (v', totv)  = scanT v
\end{code}
\vfill
Work: $O (n \log n)$, depth: $O (\log n)$.
\end{frame}

\begin{frame}{Right guess}
\begin{code}
data P a = P a a

data T a = L a | B (T (P a))
\end{code}

\vspace{5ex}
Work: $O (n)$, depth: $O (\log n)$.
\end{frame}


\nc\scanT{\text{scanᵀ}}
\nc\scanA{\text{scanᴬ}}
\nc\parse{\text{parse}}
\nc\id{\text{id}}
%% \nc\T[2]{\text{Tree}_{#1}\, #2}
\nc\BT[2]{2^{\uparrow #1}\, #2}
\nc\Arr[2]{\text{Arr}_{#1}\, #2}

\begin{frame}{Correctness}
\[\begin{tikzcd}
  \BT n a \arR{\scanT} \BT n a × a \\
  \Arr{2^n}a \arUR{\parse}{\scanA} \Arr{2^n}a × a \arU{\parse ⊗ \id}
\end{tikzcd}\]
\end{frame}

\end{document}
