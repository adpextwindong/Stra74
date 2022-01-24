import Data.Function (fix)
import Debug.Trace (trace)
import qualified Data.Map.Strict as M
import qualified Data.Map.Lazy as LM
import Data.Bifunctor

type Identifier = String
type Label = Identifier

type Store = M.Map Identifier Expr

data Command = Dummy Int
             | Sequence Command Command
             | IFE Expr Command Command
             | While Expr Command
             | CommandBlock [(Label, Command)]
             | ResultIs Expr --TODO
             | VarDecl Identifier Expr
             | Incr Identifier
             | Print Identifier
             | Goto Label
             deriving Show

data Expr = ELabel Label
          | ETrue
          | EFalse
          | Cond Expr Expr Expr
          | ValOf Command --TODO
          | ELTE Expr Expr
          | Var Identifier
          | Const Int
            deriving Show

type Cont = Store -> Store

data Env = Env {
             gotoTable :: LM.Map Label (Store -> IO Store)
           }

--Looks up an identifier
find :: Env -> Store -> Identifier -> Expr
find _ s i = s M.! i

insert :: Identifier -> Expr -> Store -> Store
insert i e s = M.insert i e s

--Expression Continuation
type K = Expr -> Cont

kTrace :: K
kTrace = \e -> trace (show e) id

--Monadic Expression Continuation (for printing in this example)
--Lox can probably lose MonadState but hold onto MonadFail/MonadIO
type KM m = Expr -> Store -> m Store

evalM :: Expr -> Env -> KM IO -> Store -> IO Store
evalM (ETrue) env k store = k ETrue store
evalM (EFalse) env k store = k EFalse store
evalM (ELabel identifier) env k store = k (find env store identifier) store
evalM (Cond e p q) env k store = condk store
  where
    condk = evalM e env (\e' s' -> case e' of
                                    ETrue -> evalM p env k s'
                                    EFalse -> evalM q env k s'
                                    _ -> error "Type Error: Cond expects tt or ff")

evalM (ELTE e1 e2) env k store = contLTE
  where
    typeError = error "Type Error: LTE expects Const Int"
    contLTE = evalM e1 env (contE1) store
    contE1 = (\e1' s' -> case e1' of
                          (Const e1i) -> evalM e2 env (contE2 e1i) s'
                          _ -> typeError)

    contE2 e1i = (\e2' s''-> case e2' of
                          (Const e2i) -> k (answer e1i e2i) s''
                          _ -> typeError)

    answer x y = case x <= y of
                   True -> ETrue
                   False -> EFalse

evalM (Var i) env k store = k (store M.! i) store
evalM c@(Const i) env k store = k c store

evalM e env k store | trace (show e) False = undefined

interpretM :: Command -> Env -> (Store -> IO Store) -> (Store -> IO Store)
--interpretM gamma env k s | trace ("interpretting " <> show gamma) False = undefined
interpretM w@(While e gamma) env k s = evalM e env whileCont s
  where whileCont e' s' = case e' of
                            ETrue -> interpretM gamma env (\s'' -> interpretM w env k s'') s'
                            EFalse -> k s'
                            _ -> error "Type Error: While expects tt/ff expr"

interpretM (Dummy i) env k s = do
    print ("interpetM Dummy" <> show i)
    k s

interpretM (VarDecl i e) env k s = k (M.insert i e s)
interpretM (Incr i) env k s = case M.lookup i s of
                                Nothing -> error $ "Missing Var Error: Identifier "<> i <> " undeclared"
                                Just e -> case e of
                                            (Const v) -> k (M.insert i (Const (v+1)) s)
                                            _ -> error $ "Type Error: Incr expects Const"
interpretM (Print i) env k s = do
  print ("interpretM Print" <> show (s M.! i))
  k s

interpretM (Sequence g g') env k s = interpretM g env (\s' -> interpretM g' env k s') s
interpretM (CommandBlock []) env k s = k s
interpretM (CommandBlock ((_,g):gs)) env k s = interpretM g env (\s' -> interpretM (CommandBlock gs) env k s') s
interpretM (IFE e gtrue gfalse) env k s =  evalM e env (\e' s' -> case e' of
                                                                 ETrue -> interpretM gtrue env k s'
                                                                 EFalse -> interpretM gfalse env k s'
                                                                 _ -> error "Type Error: IFE Expets tt/ff") s
interpretM (Goto label) env@(Env gotoTable) k s = (gotoTable LM.! label) s

idM :: Store -> IO Store
idM = return

tW = interpretM (While EFalse (Dummy 1)) blankEnv idM emptyStore
tWW = interpretM (While ETrue (Dummy 1)) blankEnv idM emptyStore
tWWW = interpretM (Sequence (VarDecl "x" (Const 1))
                            (While (ELTE (Var "x") (Const 5))
                                   (Sequence (Print "x")
                                             (Incr "x")))) blankEnv idM emptyStore

emptyStore = M.empty

--eval (ELTE (Const 4) (Const 3)) Env kTrace emptyStore

tVD = interpretM (Sequence (VarDecl "x" (Const 0))
                           (Sequence (Incr "x") (Print "x")))
                blankEnv idM emptyStore


--Traverse AST for labels and their continuations.
--Labels must be distinct otherwise an error is thrown when its evaluation is forced
labelPass :: Command -> Env -> (Store -> IO Store) -> LM.Map Label (Store -> IO Store)
labelPass (Sequence g gs) env k     = LM.union (labelPass g env gcont) (labelPass gs env k)
  where gcont = interpretM gs env k

labelPass (IFE _ l r) env k         = LM.union (labelPass l env k ) (labelPass r env k)
labelPass (While _ g) env k         = labelPass g env k
labelPass c@(CommandBlock ls) env k = LM.unionsWith distinctLabelError (topLabels : nestedLabels)
  where
    topLabels = commandBlockConts ls env k
    nestedLabels = fmap ((\g -> labelPass g env k). snd) ls

labelPass _ _ _ = LM.empty
labelPass gamma env k | trace (show gamma) False = undefined

commandBlockConts :: [(Label, Command)] -> Env -> (Store -> IO Store) -> LM.Map Label (Store -> IO Store)
commandBlockConts [] env k = LM.empty
commandBlockConts ((l,g):gs) env k = LM.union (LM.singleton l gke) (commandBlockConts gs env k)
  where gke = interpretM g env (interpretM (CommandBlock gs) env k)


insertDistinct :: (Ord k) => k -> v -> LM.Map k v -> LM.Map k v
insertDistinct = LM.insertWith (\_ _ -> error "Labels must be distinct")

--We could use set intersection to find nondistinct labels alternatively as a check

distinctLabelError = (\_ _ -> error "Labels must be distinct")

blankEnv = Env LM.empty

prog = Sequence (VarDecl "x" (Const 0))
       (Sequence (VarDecl "y" (Const 666))
       (Sequence (CommandBlock [("10" ,Incr "x"), ("20", Print "x")])
       (Sequence (IFE (ELTE (Var "x") (Const 5))
                      (Goto "10")
                      (Print "y"))
                 (Print "x"))))

testProg = interpretM prog (fixProgEnv' prog) idM emptyStore

--This is to get labelPass to reference itself, the newly created env.
fixProgEnv prog = env
  where
    env = Env (labelPass prog env kId)

fixProgEnv' prog = fix (\e -> Env (labelPass prog e kId))

kId = return :: Store -> IO Store
