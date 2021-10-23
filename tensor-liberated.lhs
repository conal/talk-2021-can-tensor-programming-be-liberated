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

\RequirePackage{tikz-cd, newunicodechar, amssymb, stmaryrd, unicode-math, setspace, comment, listings, anyfontsize}

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
\emph{Many Unix programs contain an entangled parser (from text) and unparser (to text).}
\end{frame}

\begin{frame}{Likewise,}
\vspace{10ex}
\vfill
\begin{center} \em
Many array programs contain a parser (from arrays) and unparser (to arrays).
\end{center}
\vfill
\vspace{10ex}
Disentangling improves clarity and suggests improvements.
\end{frame}

\definecolor{statColor}{rgb}{0,0,0.2}

\nc\stats[3]{\hfill \small \textcolor{statColor}{size: #1, work: #2, depth: #3}\hspace{1.5ex}}

\begin{frame}{Efficient parallel prefix (left scan) \stats{16}{27}{6}}
\begin{center}
\wpicture{5in}{lsums-lt4}
\end{center}
\end{frame}

%% \begin{frame}{Efficient parallel prefix (left scan) \stats{32}{58}{8}}
%% \begin{center}
%% \wpicture{5in}{lsums-lt5}
%% \end{center}
%% \end{frame}

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
\vspace{12ex}
\large
What is it?
\end{frame}

\nc\Q[2]{Q\ #1\ #2}
\nc\id{\text{id}}
\nc\scanA{\textit{scanA}}
\nc\Arr[2]{\textit{Arr}\ {#1}\  #2}

\nc\parseQ{\textit{parseQ}}
\nc\scanQ{\textit{scanQ}}

\begin{frame}{Clarifying the question}
\vspace{4ex}
\[\begin{tikzcd}[column sep = 13em, row sep = 8em]
  \Q d a \arR{\scanQ} \Q d a \\
  \Arr{2^d}a \arUR{\parseQ}{\scanA} \Arr{2^d}a \arU{\parseQ}
\end{tikzcd}\]\\[6ex]
Where $\scanQ$ is simple to state, prove, and generalize; and $\parseQ$ is formulaic.
\end{frame}

\begin{frame}{A compositional refinement}
\vspace{4ex}
\[\begin{tikzcd}[column sep = 12em, row sep = 8em]
  \Q d a \arR{\scanQ} \Q d a × a \\
  \Arr{2^d}a \arUR{\parseQ}{\scanA} \Arr{2^d}a × a \arU{\parseQ ⊗ \id}
\end{tikzcd}\]\\[6ex]
Where $\scanQ$ is simple to state, prove, and generalize; and $\parseQ$ is \end{frame}

\nc\down{{\scriptscriptstyle ↓}}
\nc\up{{\scriptscriptstyle ↑}}

%format Td = T"\down"
%format scanTd = scanT"\down"
%format utot = u"_{"tot"}"
%format vtot = v"_{"tot"}"

\begin{frame}{Wrong guess: top-down, binary, leaf trees}
\begin{code}
data Td :: Type -> Type where
  L  :: a -> Td a
  B  :: Td a -> Td a -> Td a
\end{code}
\vspace{-5.5ex}
\pause
\begin{code}
deriving instance Functor

SPC
scanTd :: Monoid a => Td a -> Td a × a
scanTd (L x)    = (L mempty , x)
scanTd (B u v)  = (B u' (fmap (utot ⊕) v') , utot <> vtot)
  where
    (u'  , utot  ) = scanTd u
    (v'  , vtot  ) = scanTd v
\end{code}
\hfill Work: $O (n \lg n)$, depth: $O (\lg n)$.
\end{frame}

%format Succ = Suc

\begin{frame}{Refined wrong guess: top-down, binary, \emph{perfect}, leaf trees}
\begin{textblock}{128}[1,0](350,45)
\begin{tcolorbox}
\mathindent1ex
\begin{code}
data Nat = Zero | Succ Nat
\end{code}
\end{tcolorbox}
\end{textblock}
\begin{code}
data Td :: Nat -> Type -> Type where
  L  :: a -> Td Zero a
  B  :: Td d a -> Td d a -> Td (Succ d) a

deriving instance Functor (Td d)

SPC
scanTd :: Monoid a => Td d a -> Td d a × a
scanTd (L x)    = (L mempty , x)
scanTd (B u v)  = (B u' (fmap (utot ⊕) v') , utot <> vtot)
  where
    (u'  , utot  ) = scanTd u
    (v'  , vtot  ) = scanTd v
\end{code}
\hfill Work: $O (n \lg n)$, depth: $O (\lg n)$.
\end{frame}

\begin{frame}{Work-inefficient parallel prefix (left scan) \stats{16}{33}{4}}
\begin{center}
\wpicture{5in}{lsums-rt4}
\end{center}
\end{frame}

\begin{frame}{Work-inefficient parallel prefix (left scan) \stats{32}{81}{5}}
\begin{center}
\wpicture{5in}{lsums-rt5}
\end{center}
\end{frame}

\begin{frame}{Work-inefficient parallel prefix (left scan) \stats{64}{193}{6}}
\begin{center}
\wpicture{5in}{lsums-rt6}
\end{center}
\end{frame}

\nc\scanTd{\textit{scanT}\down}
\nc\parsed{\textit{parseT}\down}
\nc\BTd[2]{T\down\ #1\ #2}

\begin{frame}{But right answer}
\[\begin{tikzcd}[column sep = 12em, row sep = 8em]
  \BTd d a \arR{\scanTd} \BTd d a × a \\
  \Arr{2^d}a \arUR{\parsed}{\scanA} \Arr{2^d}a × a \arU{\parsed ⊗ \id}
\end{tikzcd}\]
\end{frame}

\begin{frame}{Wrong guess refactored}
\vspace{-1ex}
\begin{code}
data P a = a :# a deriving Functor

data Td :: Nat -> Type -> Type where
  L  :: a -> Td Zero a
  B  :: P (Td d a) -> Td (Succ d) a
deriving instance Functor (Td d)

SPC
SPC

scanTd :: Monoid a => Td d a -> Td d a × a
scanTd (L x) = (L mempty , x)
scanTd (B (u :# v)) = (B (u' :# fmap (utot <>) v') , utot <> vtot)
  where
    (u'  , utot  ) = scanTd u
    (v'  , vtot  ) = scanTd v
    SPC
\end{code}
\end{frame}

\begin{frame}{Wrong guess refactored again}
\vspace{-1ex}
\begin{code}
data P a = a :# a deriving Functor

data Td :: Nat -> Type -> Type where
  L  :: a -> Td Zero a
  B  :: P (Td d a) -> Td (Succ d) a
deriving instance Functor (Td d)

scanP :: Monoid a => P a -> P a × a
scanP (x :# y) = (mempty :# x , y)

scanTd :: Monoid a => Td d a -> Td d a × a
scanTd (L x) = (L mempty , x)
scanTd (B ts) = (B (zipWithP tweak tots' ts'), tot)
  where
    (ts', tots)   = unzipP (fmap scanTd ts)
    (tots', tot)  = scanP tots
    tweak x       = fmap (x ⊕)
\end{code}
\end{frame}

%format Tu = T"\up"
%format scanTu = scanT"\up"
%format zipWithTu = zipWithT"\up"
%format unzipTu = unzipT"\up"

\begin{frame}{Right guess: \emph{bottom-up}, perfect, binary, leaf trees}
\vspace{-1ex}
\begin{code}
data P a = a :# a deriving Functor

data Tu :: Nat -> Type -> Type where
  L  :: a -> Tu Zero a
  B  :: Tu n (P a) -> Tu (Succ n) a
deriving instance Functor (Tu n)

scanP :: Monoid a => P a -> P a × a
scanP (x :# y) = (mempty :# x , y)

scanTu :: Monoid a => Tu n a -> Tu n a × a
scanTu (L x) = (L mempty , x)
scanTu (B ps) = (B (zipWithTu tweak tots' ps'), tot)
  where
    (ps', tots)   = unzipTu (fmap scanP ps)
    (tots', tot)  = scanTu tots
    tweak x       = fmap (x ⊕)
\end{code}\\
\hfill Work: $O (n)$, depth: $O (\lg n)$.
\end{frame}

\nc\scanTu{\textit{scanT}\up}
\nc\parseu{\textit{parseT}\up}
\nc\BTu[2]{T\up\ #1\ #2}

\begin{frame}{Work-efficient parallel prefix (left scan) \stats{16}{27}{6}}
\begin{center}
\wpicture{5in}{lsums-lt4}
\end{center}
\end{frame}

\begin{frame}{Work-efficient parallel prefix (left scan) \stats{32}{58}{8}}
\begin{center}
\wpicture{5in}{lsums-lt5}
\end{center}
\end{frame}

\begin{frame}{Work-efficient parallel prefix (left scan) \stats{64}{121}{10}}
\begin{center}
\wpicture{5in}{lsums-lt6}
\end{center}
\end{frame}

\begin{frame}{\emph{And} right answer}
\[\begin{tikzcd}[column sep = 12em, row sep = 8em]
  \BTu d a \arR{\scanTu} \BTu d a × a \\
  \Arr{2^d}a \arUR{\parseu}{\scanA} \Arr{2^d}a × a \arU{\parseu ⊗ \id}
\end{tikzcd}\]
\end{frame}

\begin{frame}{FFT}\parskip6ex
FFT decomposes similarly, yielding classic DIT \& DIF algorithms.

See \href{http://conal.net/papers/generic-parallel-functional/}{\em Generic functional parallel algorithms: Scan and FFT} (ICFP 2017).
\end{frame}

\begin{frame}{Generalizing}\parskip6ex
The heart of parallel scan/FFT is scan/FFT on |Id|, products, and compositions.

Simple decomposition yields infinite family of \emph{correct} parallel algorithms on tries.

All such tries are isomorphic to arrays (``parsing/unparsing'').
\end{frame}

%% %format ≅ = "\mathrel{\hat\cong}"

\begin{frame}{Trie algebra and isomorphisms}
\mathindent1ex
\begin{minipage}[t]{0.3\textwidth}
\begin{code}
data  U           a = U

data  Id          a = Id a

data  (f  :*  g)  a = X (f a) (g a)

data  (g  :.  f)  a = O (g (f a))
\end{code}
\end{minipage}
\pause
\hspace{0.1\textwidth}
\begin{minipage}[t]{0.4\textwidth}
\setlength{\blanklineskip}{2.3ex}
\begin{code}
Arr 0          ≅ U

Arr 1          ≅ Id

Arr (m  +  n)  ≅ Arr m  :*  Arr n

Arr (m  *  n)  ≅ Arr n  :.  Arr m
\end{code}
\end{minipage}
\vspace{8.5ex}
\end{frame}

%format +~ = "\tilde+"
%format *~ = "\tilde*"
%format zero = "\tilde0"
%format one = "\tilde1"

\begin{frame}{Trie algebra and \emph{constructive} isomorphisms}
\vspace{5ex}
\mathindent1ex
\begin{minipage}[t]{0.3\textwidth}
\begin{code}
data  U           a = U

data  Id          a = Id a

data  (f  :*  g)  a = X (f a) (g a)

data  (g  :.  f)  a = O (g (f a))
\end{code}
\end{minipage}
\hspace{0.1\textwidth}
\begin{minipage}[t]{0.4\textwidth}
\setlength{\blanklineskip}{1.9ex}
\begin{code}
zero  :: Arr 0 ≅ U

one   :: Arr 1 ≅ Id

(+~)  :: Arr m ≅ f -> Arr n ≅ g -> Arr (m  +  n) ≅ f  :*  g

(*~)  :: Arr m ≅ f -> Arr n ≅ g -> Arr (m  *  n) ≅ g  :.  f
\end{code}
\end{minipage}
\begin{center}
\begin{code}
data (≅) :: (Type -> Type) -> (Type -> Type) -> Type where
  Iso :: (forall a . f a -> g a) -> (forall a . g a -> f a) -> f ≅ g
  -- Plus isomorphism proof
\end{code}
\end{center}
\end{frame}

\end{document}
