\documentclass{article}
\usepackage[utf8]{inputenc}
\usepackage{hyperref}
\usepackage{alltt}
\usepackage{stmaryrd}
\usepackage{amsmath}

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
$S$ Domain of Machine States (Stores)\newline
$\S$ and $\S|$ Statement brackets (equivalent to Algol 60’s begin and end)

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

\subsection{valof-resultis construction}

Handling an error exit from a function (Algol 60: Type-procedure).

A jump may be called in the middle of evaluating an expression, while there may be several partial results.

We can avoid this unnecessary complication by using the \textbf{valof-resultis} construction.

In this we have an expression of the form \textbf{valof}$\gamma$ and a command of the form \textbf{resultis}$\epsilon$.

NOTE: $\gamma$ is a Command and $\epsilon$ is an expression.

\begin{verbatim}
data Expr = ... 
          | Valof Command

data Command = ...
             | Resultis Expr
\end{verbatim}

The value of an expression

\begin{alltt}
\textbf{valof}\(\S\gamma_0\)
      \(\gamma_1\)
      ...
      \textbf{resultis} \(\epsilon'\)
      ...
\end{alltt}

is found by obeying the commands $\gamma_0$,$\gamma_1$,... in a sequence until a command \textbf{resultis} $\epsilon'$ is obeyed.

The expression $\epsilon'$ is then evaluated and this value is taken as the value of the whole expression. All programming languages which allow functions to be defined have some construction which is semantically equivalent to this \textbf{resultis} command, but many languages restrict its use so that it gets confused with function definition.

The difficult type of jump is inside the body of a \textbf{valof} block to a label which is outside. The following is a small PL to illustrate the semantics of these jumps.

\section{A small "continuation" language}

\subsection {Syntactic Categories}
$\xi \in Id$ Usual Identifiers\newline
$\gamma \in Cmd$ Commands\newline
$\epsilon \in Exp$ Expressions\newline
$\phi \in Fn$ Some Primitive Commands\newline

\vspace{1.5cm}

\begin{verbatim}
\begin{code}
data Store = Store

type Cont = Store -> Store

type Identifier = String
type Label = Identifier

data Command = Prim
             | Dummy
             | Sequence Command Command
             | IFE (Expr) Command Command
             | While (Expr) Command
             | CommandBlock [(Label, Command)]
             | ResultIs (Expr)

data Expr = ELabel Label
          | ETrue
          | EFalse
          | Cond Expr Expr Expr
          | ValOf Command

\end{code}

\end{verbatim}

There is function, known as an environment which gives the mapping from identifiers to their denotations.

\begin{align*}
\rho \in Env = [Id \rightarrow D].
\end{align*}

\subsection{Continuations}

\end{document}