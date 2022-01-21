\documentclass{article}
\usepackage[utf8]{inputenc}
\usepackage{hyperref}
\usepackage{alltt}
\usepackage{stmaryrd}
\usepackage{amsmath}

\title{Tennent76 Reading}
\author{George Takumi Crary}
\begin{document}
\maketitle

\section{Common Place}
\subsection{Basic Concepts}
The role of \emph{operational} models of language semantics is to formalize language implementation methods so that their correctness may be verified by reference to standard definition \cite{milne74}

\subsection{Applicative Languages}
The characteristic semantic feature of expressions is that they are "evaluated"; that is, the semantic interpretation of an expression ultimately defines its "value." Furthermore, for "pure" expressions, it is \emph{exclusively} the value that has semantic importance. This linguistic property is termed \emph{referential transparency} \cite{quine60}, for it allows a sub-expression to be replaced by any other expression having the same value without any effect on the value of the whole. Languages or language subsets having the property of referential transparency are termed \emph{applicative}; other adjectives that have been used include declarative, denotative, descriptive, and functional.

\subsection{Environments}
\begin{verbatim}
\begin{code}

type Identifier = String

data AEXP = Base Int             -- ^ Base (Constants)
          | Var Identifier       -- ^ Identifier
          | Lam Identifier AEXP  -- ^ Abstraction
          | App AEXP AEXP        -- ^ Combination
        
\end{code}
\end{verbatim}

Assume that there is a space of expressible values \textbf{E} and an interpretation function $\mathcal{B} : \textbf{Bas} \rightarrow \textbf{E}$

TODO The mathematical definition of Y will be discussed in Section 4.3.

\begin{thebibliography}{99}

\bibitem{milne74}
\href{https://www.cs.ox.ac.uk/files/3286/PRGX13.pdf}{Milne, R.E. [1974]. \emph{The formal semantics of computer languages and their implementations.} Ph.D. Th., Cambridge U. and Tech. Microfiche TCF-2, Oxford U. Computing Lab., Programming Research Group}

\bibitem{quine60}
Quine, W.V. [1960]. \emph{Word and Object.} Technology Press, Cambridge, Mass., and Wiley, New York. 

\end{thebibliography}

\end{document}
