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

\subsection{Environments and Stores}

\subsubsection{Expressions and Environments Semantics}

Consider the following archetypal expression language AEXP (applicative expressions):

\begin{verbatim}
\begin{code}

type Identifier = String

data AEXP = Base Int             -- ^ Base (Constants)
          | Var Identifier       -- ^ Identifier
          | Lam Identifier AEXP  -- ^ Abstraction
          | App AEXP AEXP        -- ^ Combination
          | Paren AEXP           -- ^ Parenthesization
        
\end{code}
\end{verbatim}

Assume that there is a space of expressible values \textbf{E} and an interpretation function $\mathcal{B} : \textbf{Bas} \rightarrow \textbf{E}$

In order to determine the value of an expression which may in general contain free identifiers, it is necessary to know the values to which such identifers are bound in that local context. In AEXP the binding must be established by both an abstraction (which defines its textual scope) and a combination (whose operand supplies the value to be denoted); the set of associations of identifiers and their denotations in any context is termed the \emph{environment}.

A convenient model for environments is as follows:

\begin{align*}
\rho : \textbf{U} = \textbf{Ide} \rightarrow \textbf{D}
\end{align*}

where \textbf{D} is a space of denotable values, so that the value to which an identifier I is bound in the environment $\rho$ is $\rho\llbracket I \rrbracket$. In order to bind I to a value $\delta : \textbf{D}$ we use the "updating" notation of Section 2.3; that is, $\rho\llbracket\delta/I\rrbracket$ is the environment in which I is bound to $\delta$ and is otherwise the same as $\rho$.

\subsubsection{Commands and Stores Basic Model}

The meanings of commands in the simple language \emph{LOOP} were defined to be state transition functions, where a state could be modeled by a function from "variables" into their current contents. For more complex languages this simple approach is not adequate and it is necessary to structure their semantic models to incorporate both a textually determined environment (as discussed in Section 3) and a dynamically changing abstract store.

NOTE: A closure could be considered an environment seperate from the locally stored variables for Lox???

This complication arises because it is necessary to distinguish between \emph{identifiers} (program variables, symbolic names, formal parameters) which are syntactic entites, and \emph{locations} (storage variables, references, L-values, addresses, pointers) which are semantic. An identifier appears in a program and is statically bound to its denotation within the scope of its declaration or binding construction; a location is in general computed (for example, by an array indexing operation or an indirect reference via a "pointer"-valued expression), and it may even be allocated or deallocated dynamically. The contents of a location may be irreversibly updated at any point, but identifier denotations "nest" so that upon leaving an "inner" scope, the previous environment reverts back. In many languages the class of values which are \emph{denoatable} by identifiers is not the same as those which are \emph{storable} in locations; in Algol 60, for example, they are completely disjoint: only numbers and Booleans are storable, whereas the denoatable values are locations, arrays, procedures, labels, switches, strings, and parameters called "by name" \cite{strachey72}.

Sections 5.3 and 5.4 get deeper into this.

TODO The mathematical definition of Y will be discussed in Section 4.3.

\begin{thebibliography}{99}

\bibitem{milne74}
\href{https://www.cs.ox.ac.uk/files/3286/PRGX13.pdf}{Milne, R.E. [1974]. \emph{The formal semantics of computer languages and their implementations.} Ph.D. Th., Cambridge U. and Tech. Microfiche TCF-2, Oxford U. Computing Lab., Programming Research Group}

\bibitem{quine60}
Quine, W.V. [1960]. \emph{Word and Object.} Technology Press, Cambridge, Mass., and Wiley, New York. 

\bibitem{strachey72}
Strachey, C. [1972]. Varieties of programming language. Proc. International Computing Symp., Cini Foundation,  enice; also Tech. Monograph PRG-10, Oxford U. Computing Lab. Programming Research Group

\end{thebibliography}

\end{document}
