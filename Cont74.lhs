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

\section{Conventions}

$C$\newline
$\gamma$ Command\newline
$\epsilon$ Expression\newline
$\rho$ Environment\newline
$\sigma$ Store\newline
$\theta$ Store Transformation from $S \rightarrow S$\newline
$S$ Domain of Machine States (Stores)

\section{The problem of jumps}

In the semantics given in [13] the value of a command is a function which transforms the store, so that, in symbolic terms\newline

$C\llbracket\gamma\rrbracket(\rho) = \theta$\newline

where $C$ is the semantic function mapping commands to their meaning, $\gamma$ is a command, $\rho$ the environment which gives the denotations associated with the identifiers in $\gamma$ and $\theta$ is a store transformation, i.e.\newline

$\theta \in C = [S \rightarrow S]$\newline

where $S$ is the domain of machine states (or stores). We use the double brackets $\llbracket \rrbracket$ as an aid to the eye to separate the program text $\gamma$ from the value domain expression which form the rest of the equation.

\subsection{Sequencing}

The normal sequence of commands is then naturally interpreted as performing one store transformation after another, so that the overall effect is that of functional composition.
Thus, if we have two commands $\gamma_0$ and $gamma_1$ with store transformations given by\newline

$\theta_0 = C\llbracket \gamma_0 \rrbracket (\rho)$

$\theta_1 = C\llbracket \gamma_1 \rrbracket (\rho)$\newline

The effect of the sequence of commands $\gamma_0 ; \gamma_1$ on any initial store $\sigma$ will be to produce a store\newline

$\sigma^{'} = \theta_1 ( \theta_2 (\sigma))$

so that the semantic equations take the form (compositionally)\newline

$C\llbracket \gamma_0 ;\gamma_1 \rrbracket (\rho) = (C\llbracket \gamma_1 \rrbracket(\rho))\circ (C\llbracket \gamma_0 \rrbracket(\rho))$\newline

This simple scheme breaks down if $\gamma_0$ contains a jump to some external label.

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