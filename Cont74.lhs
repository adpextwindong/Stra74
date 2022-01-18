\documentclass{article}
\usepackage[utf8]{inputenc}
\usepackage{hyperref}
\usepackage{stmaryrd}

\title{Strachey74 Reading}
\author{George Takumi Crary}
\begin{document}
\maketitle


\href{https://www.cs.tufts.edu/comp/150FP/archive/christopher-strachey/continuations.pdf}{Continuations: A Mathematical Semantics for Handling Full Jumps}

References in particular:

\href{https://www.cs.ox.ac.uk/files/3228/PRG06.pdf}{Scott, D. and Strachey, C. Toward a mathematical semantics for computer languages. In Proc. of the Symposium
on Computers and Automata, Polytechnic Institute of Brooklyn, 1971. Also as Technical Monograph PRG-6, Oxford University Computing Laboratory, Programming Research Group}

\section{The problem of jumps}

In the semantics given in [13] the value of a command is a function which transforms the store, so that, in symbolic terms

$C\llbracket\gamma\rrbracket(\rho) = \theta$

where $C$ is the semantic function mappign commands to their meaning, $\gamma$ is a command, $\rho$ the environment which gives the denotations associated with the identifiers in $\gamma$ and $\theta$ is a store transformation, i.e.

$\theta \in C = [S \rightarrow S]$

where $S$ is the domain of machine states (or stores). We use the double brackets $\llbracket \rrbracket$ as an aid to the eye to seperate the program text $\gamma$ from the value domain expression which form the rest of the qeuation.

\section{A small "continuation" language}
type Identifier = String

\begin{verbatim}
\begin{code}
data Store = Store

type Cont = Store -> Store

type Identifier = String

data Command = Prim
             | Dummy
             | Sequence Command Command
             | IFE Expr Command Command
             | While Expr Command
             | Declarations [(Identifier, Command)]
             | ResultIs Expr

data Expr = Expr
\end{code}
\end{verbatim}
\end{document}