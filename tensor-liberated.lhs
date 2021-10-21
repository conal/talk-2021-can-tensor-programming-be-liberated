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

\begin{frame}{Efficient parallel prefix (left scan), 16 elements}
\begin{center}
\wpicture{5in}{lsums-lt4}
\end{center}
\end{frame}

\begin{frame}{Efficient parallel prefix (left scan), 32 elements}
\begin{center}
\wpicture{5in}{lsums-lt5}
\end{center}
\end{frame}

\begin{frame}{Efficient parallel prefix (left scan), 64 elements}
\begin{center}
\wpicture{5in}{lsums-lt6}
\end{center}
\end{frame}

\begin{frame}{An efficient array program (CUDA C)}
\vspace{0.5ex}
\hspace{1in}\wpicture{4.35in}{cuda-and-beaker}
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

\pause\vspace{4ex}
Still, why does it work, and how does it (correctly) generalize?
\end{frame}

\begin{frame}{Realization}
\Large
\vspace{11ex}
\begin{center}
\emph{It's not naturally an array algorithm.}
\end{center}
\pause
\vspace{11ex}
What is it?
\end{frame}

\rnc\SourceModule{Code}

\nc\down{{\scriptscriptstyle ↓}}
\nc\up{{\scriptscriptstyle ↑}}
\nc\Arr[2]{\textit{Arr}\ {#1}\  #2}

%format Td = T"\down"
%format scanTd = scanT"\down"
%format utot = u"_{"tot"}"
%format vtot = v"_{"tot"}"

\nc\id{\text{id}}
\nc\scanA{\textit{scanA}}

\begin{frame}{Wrong guess: top-down binary trees}
\vspace{5ex}
\begin{code}
data Td a = L a | B (Td a) (Td a)
\end{code}
\vspace{-2ex}
\pause
\begin{code}
deriving instance Functor Td

scanTd :: Monoid a => Td a -> (Td a , a)
scanTd (L x)    = (L mempty , x)
scanTd (B u v)  = (B u' ((utot SPC <>) <#> v') , utot <> vtot)
  where
    (u'  , utot  ) = scanTd u
    (v'  , vtot  ) = scanTd v
\end{code}\\
\vspace{2ex}
Work: $O (n \lg n)$, depth: $O (\lg n)$.
\end{frame}

\nc\scanTd{\textit{scanT}\up}
\nc\parsed{\textit{parse}\up}
\nc\BTd[2]{T\up\ #1\ #2}

\begin{frame}{But right answer}
\[\begin{tikzcd}[column sep = 12em, row sep = 8em]
  \BTd n a \arR{\scanTd} \BTd n a × a \\
  \Arr{2^n}a \arUR{\parsed}{\scanA} \Arr{2^n}a × a \arU{\parsed ⊗ \id}
\end{tikzcd}\]
\end{frame}

\begin{frame}{Wrong guess refactored (top-down binary trees)}
\vspace{0ex}
\begin{code}
data P a = a :# a

data Td a = L a | B (P (Td a)) deriving Functor
\end{code}
\vspace{2.5ex}
\begin{code}
scanTd :: Monoid a => Td a -> (Td a , a)
scanTd (L x) = (L mempty , x)
scanTd (B (u :# v)) = (B (u' :# (utot SPC <>) <#> v') , utot <> vtot)
  where
    (u'  , utot  ) = scanTd u
    (v'  , vtot  ) = scanTd v
\end{code}\\
\vspace{2ex}
Work: $O (n \lg n)$, depth: $O (\lg n)$.
\end{frame}

%format Tu = T"\up"
%format scanTu = scanT"\up"
%format zipWithTu = zipWith"\up"
%format unzipTu = unzip"\up"

\begin{frame}{Right guess: bottom-up, perfect binary trees}
\begin{code}
data P a = a :# a

data Tu a = L a | B (Tu (P a)) deriving Functor

scanP :: Monoid a => P a -> (P a , a)
scanP (x :# y) = (mempty :# x , y)

scanTu :: Monoid a => Tu a -> (Tu a , a)
scanTu (L  x   ) = (L mempty , x)
scanTu (B  ts  ) = (B (zipWithTu tweak tots' ts'), tot)
  where
    (ts' , tots)   = unzipTu (scanP <#> ts)
    (tots' , tot)  = scanTu tots
    tweak t        = fmap (t SPC <>)
\end{code}\\
Work: $O (n)$, depth: $O (\lg n)$.
Many easy optimizations.
\end{frame}

\nc\scanTu{\textit{scanT}\up}
\nc\parseu{\textit{parse}\up}
\nc\BTu[2]{T\up\ #1\ #2}

\begin{frame}{\emph{And} right answer}
\[\begin{tikzcd}[column sep = 12em, row sep = 8em]
  \BTu n a \arR{\scanTu} \BTu n a × a \\
  \Arr{2^n}a \arUR{\parseu}{\scanA} \Arr{2^n}a × a \arU{\parseu ⊗ \id}
\end{tikzcd}\]
\end{frame}

\end{document}
