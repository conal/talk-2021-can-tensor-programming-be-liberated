%% -*- latex -*-

%% Tweak when recording for speaker video overlay in the upper right corner
\newif\ifrecording

\recordingtrue

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
%% \setbeamertemplate{footline}{}

\setbeamersize{text margin left=.5cm,text margin right=.5cm}

\usepackage{catchfilebetweentags}
\usepackage[useregional]{datetime2}

\RequirePackage{tikz-cd, newunicodechar, amsmath, amsfonts, amssymb, stmaryrd, unicode-math, setspace, comment, listings, anyfontsize}

\input{macros}
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

% https://tex.stackexchange.com/questions/83048/change-the-contents-of-footline-in-a-beamer-presentation
\makeatletter
\setbeamertemplate{footline}
{
  \leavevmode%
  \hbox{%
  \begin{beamercolorbox}[ht=4.25ex,dp=1ex,center]{title in head/foot}%
    \usebeamerfont{title in head/foot}\vspace{0.5ex} Oxford Tensor Programming Seminar
  \end{beamercolorbox}%
  }
  \vskip0pt%
}
\makeatother

\begin{document}

\maketitle

\makeatletter
\setbeamertemplate{footline}
{
  \leavevmode%
  \hbox{%
  \begin{beamercolorbox}[wd=.2\paperwidth,ht=2.25ex,dp=1ex,center]{author in head/foot}%
    \usebeamerfont{author in head/foot}Conal Elliott
  \end{beamercolorbox}%
  \begin{beamercolorbox}[wd=.6\paperwidth,ht=2.25ex,dp=1ex,center]{title in head/foot}%
    \usebeamerfont{title in head/foot}Can Tensor Programming Be Liberated from the Fortran Data Paradigm?
  \end{beamercolorbox}%
  \begin{beamercolorbox}[wd=.2\paperwidth,ht=2.25ex,dp=1ex,right]{date in head/foot}%
    \usebeamerfont{date in head/foot}\insertshortdate{}\hspace*{2em}
    \insertframenumber{} / \inserttotalframenumber\hspace*{2ex} 
  \end{beamercolorbox}}%
  \vskip0pt%
}
\makeatother

%if False

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

%endif

%if False

\nc\bboxed[1]{\boxed{\rule[-0.9ex]{0pt}{2.8ex}#1}}
\nc\vox[1]{\bboxed{#1}}
\nc\tvox[2]{\vox{#1}\vox{#2}}

\nc\trans[1]{\\[1.3ex] #1 \\[0.75ex]}

\begin{frame}{Prefix sum (left scan)}
\begin{center}
\begin{minipage}[c]{0.3\textwidth}
\[
\begin{array}{c}
\vox{a_1, \ldots, a_n}
\trans{\Downarrow}
\tvox{b_1, \ldots, b_n}{b_{n+1}}
\end{array}
\]
\end{minipage}
where
\begin{minipage}[c]{0.3\textwidth}
\[ b_k = \sum\limits_{1 \le i < k}{a_i} \]
\end{minipage}
\end{center}
\end{frame}

%endif

\definecolor{statColor}{rgb}{0,0,0.2}

\ifrecording
\nc\stats[3]{}
\else
\nc\stats[3]{\hfill \small \textcolor{statColor}{size: #1, work: #2, depth: #3}\hspace{1.5ex}}
\fi

\begin{frame}{Linear prefix sum (left scan) \stats{16}{15}{15}}
\begin{center}
\ifrecording
\vspace{3ex}
\wpicture{0.85\textwidth}{lsums-lv16}\hspace{0.1\textwidth}{\ }
\else
\wpic{lsums-lv16}
\fi
\end{center}
\end{frame}

\begin{frame}{Efficient \emph{parallel} prefix (left scan) \stats{16}{27}{6}}
\begin{center}
\wpic{lsums-lt4}
\end{center}

\vspace{-5.5ex}
\hfill Work: $O (n)$, depth: $O (\lg n)$.\\[0ex]{\ }
\end{frame}

%% \begin{frame}{Efficient parallel prefix (left scan) \stats{32}{58}{8}}
%% \begin{center}
%% \wpic{lsums-lt5}
%% \end{center}
%% \end{frame}

\begin{frame}{An efficient array program (CUDA C)}
\vspace{0.5ex}
\hspace{10ex}\wpicture{4.35in}{cuda-and-beaker}
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

\nc\Z[2]{Z\ #1\ #2}
\nc\id{\text{id}}
\nc\scanA{\textit{scanA}}
\nc\Arr[2]{\textit{Arr}\ {#1}\  #2}

\nc\parseZ{\textit{parseZ}}
\nc\scanZ{\textit{scanZ}}

\begin{frame}{Clarifying the question}
\vspace{21ex}
\[\begin{tikzcd}[column sep = 13em, row sep = 8em]
  \Arr{2^d}a \arR{\scanA} \Arr{2^d}a
\end{tikzcd}\]
\end{frame}

\begin{frame}{Clarifying the question}
\vspace{4ex}
\[\begin{tikzcd}[column sep = 13em, row sep = 8em]
  \Z d a \arR{\scanZ} \Z d a \\
  \Arr{2^d}a \arR{\scanA} \Arr{2^d}a
\end{tikzcd}\]\\[6ex]
\textcolor{white}{Where $\scanZ$ is simple to state, prove, and generalize; and $\parseZ$ is formulaic in |Z|.}
\end{frame}

\begin{frame}{Clarifying the question}
\vspace{4ex}
\[\begin{tikzcd}[column sep = 13em, row sep = 8em]
  \Z d a \arR{\scanZ} \Z d a \\
  \Arr{2^d}a \arUR{\parseZ}{\scanA} \Arr{2^d}a \arU{\parseZ}
\end{tikzcd}\]\\[6ex]
\pause
Where $\scanZ$ is simple to state, prove, and generalize; and $\parseZ$ is formulaic in |Z|.
\end{frame}

\begin{frame}{A compositional refinement}
\vspace{4ex}
\[\begin{tikzcd}[column sep = 13em, row sep = 8em]
  \Z d a \arR{\scanZ} \Z d a × a \\
  \Arr{2^d}a \arUR{\parseZ}{\scanA} \Arr{2^d}a × a \arU{\parseZ ⊗ \id}
\end{tikzcd}\]\\[6ex]
Where $\scanZ$ is simple to state, prove, and generalize; and $\parseZ$ is formulaic in |Z|.
\end{frame}

\nc\down{{\scriptscriptstyle ↓}}
\nc\up{{\scriptscriptstyle ↑}}

%format Td = T"\down"
%format scanTd = scanT"\down"
%format utot = u"_{"tot"}"
%format vtot = v"_{"tot"}"

%format Type = "\ast"

\begin{frame}{Wrong guess: top-down, binary trees}
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

\begin{frame}{Refined wrong guess: top-down, binary, \emph{perfect} trees}
\begin{code}
data Td :: Nat -> Type -> Type where
  L  :: a -> Td 0 a
  B  :: Td d a -> Td d a -> Td (d + 1) a

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

\begin{frame}{Top-down tree scan \stats{16}{32}{4}}
\begin{center}
\wpic{lsums-rt4}
\end{center}
\end{frame}

\begin{frame}{Top-down tree scan \stats{32}{80}{5}}
\begin{center}
\wpic{lsums-rt5}
\end{center}
\end{frame}

\begin{frame}{Top-down tree scan \stats{64}{192}{6}}
\begin{center}
\wpic{lsums-rt6}
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

\begin{frame}{Refined wrong guess: top-down, binary, \emph{perfect} trees}
\vspace{-1.9ex}
\begin{code}
SPC
SPC
data Td :: Nat -> Type -> Type where
  L  :: a -> Td 0 a
  B  :: Td d a -> Td d a -> Td (d + 1) a
deriving instance Functor (Td d)

SPC
SPC

scanTd :: Monoid a => Td d a -> Td d a × a
scanTd (L x) = (L mempty , x)
scanTd (B u v) = (B u' (fmap (utot ⊕) v') , utot <> vtot)
  where
    (u'  , utot  ) = scanTd u
    (v'  , vtot  ) = scanTd v
    SPC
\end{code}
\hfill Work: $O (n \lg n)$, depth: $O (\lg n)$.
\end{frame}

\begin{frame}{Wrong guess refactored}
\vspace{-1ex}
\begin{code}
data P a = a :# a deriving Functor

data Td :: Nat -> Type -> Type where
  L  :: a -> Td 0 a
  B  :: P (Td d a) -> Td (d + 1) a
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

\begin{frame}{Wrong guess: top-down, binary, perfect trees}
\vspace{-1ex}
\begin{code}
data P a = a :# a deriving Functor

data Td :: Nat -> Type -> Type where
  L  :: a -> Td 0 a
  B  :: P (Td d a) -> Td (d + 1) a
deriving instance Functor (Td d)

scanP :: Monoid a => P a -> P a × a
scanP (x :# y) = (mempty :# x , x <> y)

scanTd :: Monoid a => Td d a -> Td d a × a
scanTd (L x) = (L mempty , x)
scanTd (B ts) = (B (zipWithP tweak tots' ts'), tot)
  where
    (ts', tots)   = unzipP (fmap scanTd ts)
    (tots', tot)  = scanP tots
    tweak x       = fmap (x ⊕)
\end{code}\\[-5ex]
\hfill Work: $O (n \lg n)$, depth: $O (\lg n)$.
\end{frame}

%format Tu = T"\up"
%format scanTu = scanT"\up"
%format zipWithTu = zipWithT"\up"
%format unzipTu = unzipT"\up"

\begin{frame}{Right guess: \emph{bottom-up}, perfect, binary trees}
\vspace{-1ex}
\begin{code}
data P a = a :# a deriving Functor

data Tu :: Nat -> Type -> Type where
  L  :: a -> Tu 0 a
  B  :: Tu d (P a) -> Tu (d + 1) a
deriving instance Functor (Tu d)

scanP :: Monoid a => P a -> P a × a
scanP (x :# y) = (mempty :# x , x <> y)

scanTu :: Monoid a => Tu d a -> Tu d a × a
scanTu (L x) = (L mempty , x)
scanTu (B ps) = (B (zipWithTu tweak tots' ps'), tot)
  where
    (ps', tots)   = unzipTu (fmap scanP ps)
    (tots', tot)  = scanTu tots
    tweak x       = fmap (x ⊕)
\end{code}\\[-5ex]
\hfill Work: $O (n)$, depth: $O (\lg n)$.
\end{frame}

\nc\scanTu{\textit{scanT}\up}
\nc\parseu{\textit{parseT}\up}
\nc\BTu[2]{T\up\ #1\ #2}

\begin{frame}{\emph{And} right answer}
\[\begin{tikzcd}[column sep = 12em, row sep = 8em]
  \BTu d a \arR{\scanTu} \BTu d a × a \\
  \Arr{2^d}a \arUR{\parseu}{\scanA} \Arr{2^d}a × a \arU{\parseu ⊗ \id}
\end{tikzcd}\]
\end{frame}

\begin{frame}{Top-down tree scan \stats{16}{32}{4}}
\begin{center}
\wpic{lsums-rt4}
\end{center}
\end{frame}

\begin{frame}{Bottom-up tree scan \stats{16}{26}{6}}
\begin{center}
\wpic{lsums-lt4}
\end{center}
\end{frame}

\begin{frame}{Top-down tree scan \stats{32}{80}{5}}
\begin{center}
\wpic{lsums-rt5}
\end{center}
\end{frame}

\begin{frame}{Bottom-up tree scan \stats{32}{57}{8}}
\begin{center}
\wpic{lsums-lt5}
\end{center}
\end{frame}

\begin{frame}{Top-down tree scan \stats{64}{192}{6}}
\begin{center}
\wpic{lsums-rt6}
\end{center}
\end{frame}

\begin{frame}{Bottom-up tree scan \stats{64}{120}{10}}
\begin{center}
\wpic{lsums-lt6}
\end{center}
\end{frame}

\begin{frame}{FFT}\parskip6ex
FFT decomposes similarly, yielding classic DIT \& DIF algorithms.

See \href{http://conal.net/papers/generic-parallel-functional/}{\em Generic functional parallel algorithms: Scan and FFT} (ICFP 2017).
\end{frame}

\begin{frame}{Top-down tree FFT (DIT) \stats{16}{188}{8}}
\begin{center}
\wpic{fft-rb4}
\end{center}
\end{frame}

\begin{frame}{Bottom-up tree FFT (DIF) \stats{16}{188}{8}}
\begin{center}
\wpic{fft-lb4}
\end{center}
\end{frame}

\begin{frame}{Top-down tree FFT (DIT) \stats{32}{524}{11}}
\begin{center}
\wpic{fft-rb5}
\end{center}
\end{frame}

\begin{frame}{Bottom-up tree FFT (DIF) \stats{32}{524}{11}}
\begin{center}
\wpic{fft-lb5}
\end{center}
\end{frame}

\begin{frame}{Generalizing}\parskip6ex
Re-express parallel algorithm via singletons, products, and \emph{compositions}.

Recomposing yields infinite family of \emph{correct} parallel algorithms on tries.

All such tries are isomorphic to arrays (``parsing/unparsing'').
\end{frame}

\begin{frame}{Arrays are numeric exponentials}
\begin{eqnarray*}
a^0 &=& 1 \\[2ex]
a^1 &=& a \\[2ex]
a^{m + n} &=& a^m × a^n \\[2ex]
a^{m \times n} &=& (a^m)^n
\end{eqnarray*}
\end{frame}

\begin{frame}{Tries are general exponentials}
\mathindent1ex
\begin{minipage}[t]{0.3\textwidth}
\begin{code}
data  U           a = U

data  I           a = I a

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

Arr 1          ≅ I

Arr (m  +  n)  ≅ Arr m  :*  Arr n

Arr (m  ×  n)  ≅ Arr n  :.  Arr m
\end{code}
\end{minipage}
\vspace{8.5ex}
\end{frame}

%format +~ = "\tilde+"
%format *~ = "\tilde*"
%format zero = "\tilde0"
%format one = "\tilde1"

\begin{frame}{Tries are general exponentials}
\vspace{4.6ex}
\mathindent1ex
\begin{minipage}[t]{0.3\textwidth}
\begin{code}
data  U           a = U

data  I           a = I a

data  (f  :*  g)  a = X (f a) (g a)

data  (g  :.  f)  a = O (g (f a))
\end{code}
\end{minipage}
\hspace{0.1\textwidth}
\begin{minipage}[t]{0.4\textwidth}
\setlength{\blanklineskip}{1.9ex}
\begin{code}
zero  :: Arr 0 ≅ U

one   :: Arr 1 ≅ I

(+~)  :: Arr m ≅ f -> Arr n ≅ g -> Arr (m  +  n) ≅ f  :*  g

(*~)  :: Arr m ≅ f -> Arr n ≅ g -> Arr (m  *  n) ≅ g  :.  f
\end{code}
\end{minipage}
\begin{center}
\begin{code}
-- ``Parse/unparse''
data (≅) :: (Type -> Type) -> (Type -> Type) -> Type where
  Iso :: (forall a . f a -> g a) -> (forall a . g a -> f a) -> f ≅ g
  -- Plus isomorphism proof
\end{code}
\end{center}
\end{frame}

\begin{frame}{Vectors}
\vspace{0.7ex}
\begin{center}
\Large $\bar n = \overbrace{I \times \cdots \times I\:}^{n \text{~times}}$
\end{center}

Right-associated (``cons''):
\begin{code}
type family (RVec d) where
  RVec 0        = U
  RVec (d + 1)  = I :* RVec d
\end{code}

Left-associated (``snoc''):
\begin{code}
type family (LVec d) where
  LVec Z        = U
  LVec (d + 1)  = LVec d :* I
\end{code}
\end{frame}

\begin{frame}{Perfect trees}
\begin{center}
\Large $h^d = \overbrace{h \circ \cdots \circ h\:}^{d \text{~times}}$
\end{center}

Right-associated (``top-down''):
\begin{code}
type family (RPow h d) where
  RPow h 0        = I
  RPow h (d + 1)  = h :. RPow h d
\end{code}
Left-associated (``bottom-up''):
\begin{code}
type family (LPow h d) where
  LPow h 0        = I
  LPow h (d + 1)  = LPow h d :. h
\end{code}

\end{frame}

%format Bush' = "\Varid{Bush}"
%format twod = "2^d"

\begin{frame}{Bushes}
\begin{code}
type family (Bush d) where
  Bush 0      = Pair
  Bush (d + 1)  = Bush d :. Bush d
\end{code}

Notes:
\rnc{\baselinestretch}{1.4}
\begin{itemize}
\item Variation of |Bush'| type in \href{http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.184.8120}{\emph{Nested Datatypes}} by Bird \& Meertens.
\item Size $2^{2^n}$, i.e., $2, 4, 16, 256, 65536, \ldots$.
\item Composition-balanced counterpart to |LPow 2 twod| and |RPow 2 twod|.
\item Easily generalizes beyond pairing and squaring.
\end{itemize}
\end{frame}

\begin{frame}{Bush scan \stats{16}{29}{5}}
\begin{center}
\wpic{lsums-bush2}
\end{center}
\end{frame}

\begin{frame}{Bush FFT \stats{16}{176}{6}}
\begin{center}
\wpic{fft-bush2}
\end{center}
\end{frame}

\begin{frame}{Scan comparison}
Size 16:\\[2ex]
\scanStats{
  \scanStat{|RPow 2 N4|}{32}{4}
  \scanStat{|LPow 2 N4|}{26}{6}
  \scanStat{|Bush   N2|}{29}{5}
}

\vspace{2ex}
Size 256:\\[2ex]
\scanStats{
  \scanStat{|RPow 2 N8|}{1024}{8}
  \scanStat{|LPow 2 N8|}{502}{14}
  \scanStat{|Bush   N3|}{718}{10}
}

\end{frame}

\begin{frame}{FFT comparison}
Size 16:\\[2ex]
\fftStats{
  \fftStat{|RPow 2 N4|}{74}{40}{74}{188}{8}
  \fftStat{|LPow 2 N4|}{74}{40}{74}{188}{8}
  \fftStat{|Bush   N2|}{72}{32}{72}{176}{6}
}

\vspace{2ex}
Size 256:\\[2ex]
\fftStats{
  \fftStat{|RPow 2 N8|}{2690}{2582}{2690}{7692}{20}
  \fftStat{|LPow 2 N8|}{2690}{2582}{2690}{7692}{20}
  \fftStat{|Bush   N3|}{2528}{1922}{2528}{6978}{14}
}
\end{frame}

\begin{frame}{``But computer memory is arrays!''}
\begin{itemize}\parskip10ex
\pause
\item Systematically convert natural algorithms to accommodate.
\pause
\item It's really |LPow 2 d| or |RPow 2 d|.
\end{itemize}
\end{frame}

\begin{frame}{Conclusions}
\rnc{\baselinestretch}{1.8}
\begin{itemize}
\item Alternative to array programming:
  \begin{itemize}
  %% \item Elegantly compositional.
  \item Reveals algorithm essence, connections, and generalizations.
  \item Free of index computations (safe and uncluttered).
  \item Translates to array program safely and systematically.
  \end{itemize}
\pause
\item Four well-known parallel algorithms: |RPow h d|, |LPow h d|. % perfect trees
\item Two new and possibly useful algorithms: |Bush d|. % bushes
\pause
\item Other examples: arithmetic, linear algebra, polynomials, bitonic sort.
\pause
\item \emph{Optimization matters but harms clarity and composability, so do it late}.
\end{itemize}
\end{frame}

\end{document}
