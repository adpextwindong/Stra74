{-

# [Continuations: A Mathematical Semantics for Handling Full Jumps](https://www.cs.tufts.edu/comp/150FP/archive/christopher-strachey/continuations.pdf)

References in particular:

- [ Scott, D. and Strachey, C. Toward a mathematical semantics for computer languages. In Proc. of the Symposium
on Computers and Automata, Polytechnic Institute of Brooklyn, 1971. Also as Technical Monograph PRG-6, Oxford University Computing Laboratory, Programming Research Group](https://www.cs.ox.ac.uk/files/3228/PRG06.pdf)

-}

-- 3. A small "continuation" language
type Identifier = String

data Store = Store

type Cont = Store -> Store

data Command = Prim
             | Dummy
             | Sequence Command Command
             | IFE Expr Command Command
             | While Expr Command
             | Declarations [(Identifier, Command)]
             | ResultIs Expr

data Expr = Expr
