{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Typecheck
  ( TypeError (..),
    Scheme (..),
    inferProgram,
    inferExprInEmpty,
  )
where

import Ast
import Control.Monad (foldM, when)
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T

type TVarId = Int

data IType
  = TV TVarId
  | TIntT
  | TBoolT
  | TStringT
  | TNullT
  | TFunT [IType] IType
  | TArrayT IType
  | TObjectT (M.Map Text IType)
  | TCon Text [IType]
  deriving (Eq, Ord)

instance Show IType where
  show = \case
    TV n -> "t" <> show n
    TIntT -> "Int"
    TBoolT -> "Bool"
    TStringT -> "String"
    TNullT -> "Null"
    TFunT as r -> "(" <> unwords (map show as) <> ") -> " <> show r
    TArrayT t -> "[" <> show t <> "]"
    TObjectT fs -> "{ " <> unwords [T.unpack k <> ": " <> show v | (k, v) <- M.toList fs] <> " }"
    TCon n ts -> T.unpack n <> "<" <> unwords (map show ts) <> ">"

data Scheme = Forall [TVarId] IType
  deriving (Eq, Show)

-- | Each binding stores its type scheme and whether it is mutable.
type TypeEnv = M.Map Text (Scheme, Mutability)

newtype Subst = Subst (M.Map TVarId IType)
  deriving (Eq, Show, Semigroup, Monoid)

emptySubst :: Subst
emptySubst = Subst M.empty

compose :: Subst -> Subst -> Subst
compose s1@(Subst a) (Subst b) =
  Subst (M.map (apply s1) b <> a)

class Types a where
  ftv :: a -> S.Set TVarId
  apply :: Subst -> a -> a

instance Types IType where
  ftv = \case
    TV n -> S.singleton n
    TIntT -> mempty
    TBoolT -> mempty
    TStringT -> mempty
    TNullT -> mempty
    TFunT as r -> S.unions (map ftv (r : as))
    TArrayT t -> ftv t
    TObjectT fs -> S.unions (map ftv (M.elems fs))
    TCon _ ts -> S.unions (map ftv ts)

  apply (Subst s) t = case t of
    TV n -> M.findWithDefault t n s
    TIntT -> TIntT
    TBoolT -> TBoolT
    TStringT -> TStringT
    TNullT -> TNullT
    TFunT as r -> TFunT (map go as) (go r)
    TArrayT x -> TArrayT (go x)
    TObjectT fs -> TObjectT (M.map go fs)
    TCon n ts -> TCon n (map go ts)
    where
      go = apply (Subst s)

instance Types Scheme where
  ftv (Forall as t) = ftv t `S.difference` S.fromList as
  apply (Subst s) (Forall as t) =
    let s' = Subst (foldr M.delete s as)
     in Forall as (apply s' t)

instance Types (Scheme, Mutability) where
  ftv (sch, _) = ftv sch
  apply s (sch, m) = (apply s sch, m)

instance Types TypeEnv where
  ftv env = S.unions (map ftv (M.elems env))
  apply s = M.map (apply s)

data TypeError
  = UnificationFail IType IType
  | InfiniteType TVarId IType
  | UnboundVariable Text
  | MissingField Text
  | DuplicateBinding Text
  | ImmutableAssign Text
  | OtherTypeError Text
  deriving (Eq, Show)

data InferState = InferState
  { count :: Int
  }

newtype Infer a = Infer {unInfer :: ExceptT TypeError (State InferState) a}
  deriving (Functor, Applicative, Monad, MonadError TypeError, MonadState InferState)

runInfer :: Infer a -> Either TypeError a
runInfer m = evalState (runExceptT (unInfer m)) (InferState 0)

fresh :: Infer IType
fresh = do
  st <- get
  put st {count = count st + 1}
  pure (TV (count st))

instantiate :: Scheme -> Infer IType
instantiate (Forall vars t) = do
  reps <- mapM (const fresh) vars
  let s = Subst (M.fromList (zip vars reps))
  pure (apply s t)

generalize :: TypeEnv -> IType -> Scheme
generalize env t =
  let envFtv = S.unions [ftv sch | (sch, _) <- M.elems env]
      vars = S.toList (ftv t `S.difference` envFtv)
   in Forall vars t

unify :: IType -> IType -> Infer Subst
unify t1 t2 = case (t1, t2) of
  (TFunT as1 r1, TFunT as2 r2)
    | length as1 == length as2 -> unifyMany (as1 <> [r1]) (as2 <> [r2])
    | otherwise -> throwError (UnificationFail t1 t2)
  (TV v, t) -> bind v t
  (t, TV v) -> bind v t
  (TIntT, TIntT) -> pure emptySubst
  (TBoolT, TBoolT) -> pure emptySubst
  (TStringT, TStringT) -> pure emptySubst
  (TNullT, TNullT) -> pure emptySubst
  (TArrayT a, TArrayT b) -> unify a b
  (TObjectT fa, TObjectT fb)
    | M.keysSet fa == M.keysSet fb ->
        unifyMany (M.elems fa) (M.elems fb)
    | otherwise -> throwError (UnificationFail t1 t2)
  (TCon n1 as1, TCon n2 as2)
    | n1 == n2 && length as1 == length as2 -> unifyMany as1 as2
    | otherwise -> throwError (UnificationFail t1 t2)
  _ -> throwError (UnificationFail t1 t2)

unifyMany :: [IType] -> [IType] -> Infer Subst
unifyMany [] [] = pure emptySubst
unifyMany (t1 : ts1) (t2 : ts2) = do
  s1 <- unify t1 t2
  s2 <- unifyMany (map (apply s1) ts1) (map (apply s1) ts2)
  pure (compose s2 s1)
unifyMany _ _ = throwError (OtherTypeError "arity mismatch in unifyMany")

bind :: TVarId -> IType -> Infer Subst
bind a t
  | t == TV a = pure emptySubst
  | a `S.member` ftv t = throwError (InfiniteType a t)
  | otherwise = pure (Subst (M.singleton a t))

fromSurfaceType :: Type -> Infer IType
fromSurfaceType = \case
  TInt -> pure TIntT
  TBool -> pure TBoolT
  TString -> pure TStringT
  TVar _ -> fresh -- surface named vars treated as fresh unknowns
  TArray t -> TArrayT <$> fromSurfaceType t
  TObject fields -> TObjectT . M.fromList <$> mapM go fields
    where
      go (k, v) = (k,) <$> fromSurfaceType v
  TApp n args -> TCon n <$> mapM fromSurfaceType args
  TFun as r -> TFunT <$> mapM fromSurfaceType as <*> fromSurfaceType r

--------------------------------------------------------------------------------
-- Inference for expressions
--------------------------------------------------------------------------------

inferExpr :: TypeEnv -> Expr -> Infer (Subst, IType)
inferExpr env = \case
  EVar x ->
    case M.lookup x env of
      Nothing -> throwError (UnboundVariable x)
      Just (sch, _) -> do t <- instantiate sch; pure (emptySubst, t)
  ELit l -> case l of
    LInt _ -> pure (emptySubst, TIntT)
    LBool _ -> pure (emptySubst, TBoolT)
    LString _ -> pure (emptySubst, TStringT)
    LNull -> pure (emptySubst, TNullT)
  EParens e -> inferExpr env e
  EArray es -> do
    tv <- fresh
    (s, ts) <- inferList env es
    s' <-
      foldM
        ( \acc t -> do
            u <- unify (apply acc tv) (apply acc t)
            pure (compose u acc)
        )
        s
        ts
    pure (s', apply s' (TArrayT tv))
  EObject fs -> do
    (s, typed) <- foldM step (emptySubst, []) fs
    pure (s, TObjectT (M.fromList typed))
    where
      step (sAcc, out) (k, e) = do
        (sE, tE) <- inferExpr (apply sAcc env) e
        let sAll = compose sE sAcc
        pure (sAll, out <> [(k, apply sAll tE)])
  ELam params mRet body -> do
    -- build parameter types
    (env', ptys) <- foldM addParam (env, []) params
    (sBody, tBody) <- inferExpr env' body

    -- optional return type annotation
    sRet <- case mRet of
      Nothing -> pure emptySubst
      Just rt -> do
        rt' <- fromSurfaceType rt
        unify (apply sBody tBody) rt'

    let sAll = compose sRet sBody
    pure (sAll, TFunT (map (apply sAll) ptys) (apply sAll tBody))
    where
      addParam (e, ts) (Param n mt) = do
        t <- maybe fresh fromSurfaceType mt
        pure (M.insert n (Forall [] t, Immutable) e, ts <> [t])
  ECall fn args -> do
    (sFn, tFn) <- inferExpr env fn
    (sArgs, argTs) <- inferArgList (apply sFn env) args
    ret <- fresh
    sCall <- unify (apply sArgs tFn) (TFunT argTs ret)
    let sAll = compose sCall (compose sArgs sFn)
    pure (sAll, apply sAll ret)
  EMember obj field -> do
    (sObj, tObj0) <- inferExpr env obj
    let tObj = apply sObj tObj0
    case tObj of
      TObjectT fs ->
        case M.lookup field fs of
          Just tf -> pure (sObj, tf)
          Nothing -> throwError (MissingField field)
      -- fallback for unknown/non-concrete object terms
      _ -> do
        tv <- fresh
        sField <- unify tObj (TObjectT (M.singleton field tv))
        let sAll = compose sField sObj
        pure (sAll, apply sAll tv)
  EIndex arr ix -> do
    (s1, tArr) <- inferExpr env arr
    (s2, tIx) <- inferExpr (apply s1 env) ix
    sIx <- unify tIx TIntT
    tv <- fresh
    sArr <- unify (apply sIx tArr) (TArrayT tv)
    let sAll = compose sArr (compose sIx (compose s2 s1))
    pure (sAll, apply sAll tv)
  EAssign l r -> do
    -- Guard: only mutable bindings may be assigned to
    case l of
      EVar x -> case M.lookup x env of
        Just (_, Immutable) -> throwError (ImmutableAssign x)
        _                   -> pure ()
      _ -> pure ()  -- member/index assigns are permitted
    (s1, tl) <- inferExpr env l
    (s2, tr) <- inferExpr (apply s1 env) r
    s3 <- unify (apply s2 tl) tr
    let sAll = compose s3 (compose s2 s1)
    pure (sAll, apply sAll tr)
  EUnary op e -> do
    (s, t) <- inferExpr env e
    case op of
      Neg -> do
        s' <- unify t TIntT
        pure (compose s' s, TIntT)
      Not -> do
        s' <- unify t TBoolT
        pure (compose s' s, TBoolT)
  EBinary op a b -> do
    (s1, ta) <- inferExpr env a
    (s2, tb) <- inferExpr (apply s1 env) b
    let s12 = compose s2 s1
    case op of
      Add -> numNum ta tb s12 TIntT
      Sub -> numNum ta tb s12 TIntT
      Mul -> numNum ta tb s12 TIntT
      Div -> numNum ta tb s12 TIntT
      Mod -> numNum ta tb s12 TIntT
      Lt -> ordBool ta tb s12
      Lte -> ordBool ta tb s12
      Gt -> ordBool ta tb s12
      Gte -> ordBool ta tb s12
      Eq -> eqBool ta tb s12
      Neq -> eqBool ta tb s12
      And -> boolBool ta tb s12
      Or -> boolBool ta tb s12
    where
      numNum ta tb s ret = do
        sA <- unify (apply s ta) TIntT
        sB <- unify (apply sA (apply s tb)) TIntT
        let sAll = compose sB (compose sA s)
        pure (sAll, ret)

      boolBool ta tb s = do
        sA <- unify (apply s ta) TBoolT
        sB <- unify (apply sA (apply s tb)) TBoolT
        pure (compose sB (compose sA s), TBoolT)

      ordBool ta tb s = do
        sA <- unify (apply s ta) TIntT
        sB <- unify (apply sA (apply s tb)) TIntT
        pure (compose sB (compose sA s), TBoolT)

      eqBool ta tb s = do
        sA <- unify (apply s ta) (apply s tb)
        pure (compose sA s, TBoolT)
  EIfExpr c t f -> do
    (s1, tc) <- inferExpr env c
    sBool <- unify tc TBoolT
    let s1' = compose sBool s1
    (s2, tt) <- inferExpr (apply s1' env) t
    (s3, tf) <- inferExpr (apply s2 (apply s1' env)) f
    s4 <- unify (apply s3 tt) tf
    let sAll = compose s4 (compose s3 (compose s2 s1'))
    pure (sAll, apply sAll tf)

inferList :: TypeEnv -> [Expr] -> Infer (Subst, [IType])
inferList env = foldM step (emptySubst, [])
  where
    step (sAcc, ts) e = do
      (s, t) <- inferExpr (apply sAcc env) e
      let sAll = compose s sAcc
      pure (sAll, ts <> [apply sAll t])

inferArgList :: TypeEnv -> [Arg] -> Infer (Subst, [IType])
inferArgList env args = inferList env [e | Arg e <- args]

inferStmt :: TypeEnv -> Stmt -> Infer (Subst, TypeEnv)
inferStmt env = \case
  SExpr e -> do
    (s, _) <- inferExpr env e
    pure (s, apply s env)
  SLet mut name mTy rhs -> do
    when (M.member name env) $
      throwError (DuplicateBinding name)

    (s1, tRhs) <- inferExpr env rhs
    s2 <- case mTy of
      Nothing -> pure emptySubst
      Just ann -> do
        tAnn <- fromSurfaceType ann
        unify (apply s1 tRhs) tAnn
    let sAll = compose s2 s1
        env' = apply sAll env
        tFinal = apply sAll tRhs
        sch = generalize env' tFinal
    pure (sAll, M.insert name (sch, mut) env')
  SReturn _ ->
    -- Statement-level return checking can be made function-context-sensitive later.
    pure (emptySubst, env)
  SIf cond th el -> do
    (s1, tCond) <- inferExpr env cond
    sCond <- unify tCond TBoolT
    let sBase = compose sCond s1
    (sTh, envTh) <- inferBlock (apply sBase env) th
    (sEl, envEl) <- case el of
      Nothing -> pure (emptySubst, apply sTh envTh)
      Just b -> inferBlock (apply sTh envTh) b
    let sAll = compose sEl (compose sTh sBase)
        envMerged = envEl
    pure (sAll, envMerged)
  SWhile cond body -> do
    (s1, tCond) <- inferExpr env cond
    s2 <- unify tCond TBoolT
    (s3, env') <- inferBlock (apply (compose s2 s1) env) body
    pure (compose s3 (compose s2 s1), env')
  SBlock b -> inferBlock env b
  SFun name params mRet body -> do
    paramTypes <- mapM (\(Param _ mt) -> maybe fresh fromSurfaceType mt) params
    retType <- maybe fresh fromSurfaceType mRet
    let funType = TFunT paramTypes retType
        envRec = M.insert name (Forall [] funType, Immutable) env
        envParams =
          foldl
            (\e (Param n _, t) -> M.insert n (Forall [] t, Immutable) e)
            envRec
            (zip params paramTypes)

    (sBody, _) <- inferBlockWithRet (Just retType) envParams body

    let funTypeFinal = apply sBody funType
        envApplied = apply sBody env
        sch = generalize envApplied funTypeFinal
        envOut = M.insert name (sch, Immutable) envApplied

    pure (sBody, envOut)

inferStmtWithRet :: Maybe IType -> TypeEnv -> Stmt -> Infer (Subst, TypeEnv)
inferStmtWithRet mRet env = \case
  SReturn me ->
    case (mRet, me) of
      (Nothing, _) ->
        -- top-level return (or unsupported context)
        pure (emptySubst, env)
      (Just rt, Nothing) -> do
        s <- unify rt TNullT
        pure (s, apply s env)
      (Just rt, Just e) -> do
        (s1, te) <- inferExpr env e
        s2 <- unify (apply s1 te) (apply s1 rt)
        let sAll = compose s2 s1
        pure (sAll, apply sAll env)
  SBlock b ->
    inferBlockWithRet mRet env b
  SIf cond th el -> do
    (s1, tCond) <- inferExpr env cond
    sCond <- unify tCond TBoolT
    let sBase = compose sCond s1
    (sTh, envTh) <- inferBlockWithRet mRet (apply sBase env) th
    (sEl, envEl) <- case el of
      Nothing -> pure (emptySubst, envTh)
      Just b -> inferBlockWithRet mRet envTh b
    let sAll = compose sEl (compose sTh sBase)
    pure (sAll, apply sAll envEl)
  SWhile cond body -> do
    (s1, tCond) <- inferExpr env cond
    s2 <- unify tCond TBoolT
    (s3, env') <- inferBlockWithRet mRet (apply (compose s2 s1) env) body
    pure (compose s3 (compose s2 s1), env')

  -- defer to existing behavior for others
  other -> inferStmt env other

inferBlockWithRet :: Maybe IType -> TypeEnv -> Block -> Infer (Subst, TypeEnv)
inferBlockWithRet mRet env (Block ss) =
  foldM step (emptySubst, env) ss
  where
    step (sAcc, eAcc) st = do
      (s, e) <- inferStmtWithRet mRet (apply sAcc eAcc) st
      pure (compose s sAcc, e)

inferBlock :: TypeEnv -> Block -> Infer (Subst, TypeEnv)
inferBlock env (Block ss) = foldM step (emptySubst, env) ss
  where
    step (sAcc, eAcc) st = do
      (s, e) <- inferStmt (apply sAcc eAcc) st
      pure (compose s sAcc, e)

inferProgram :: Program -> Either TypeError TypeEnv
inferProgram (Program stmts) = runInfer $ do
  (_, envOut) <- foldM step (emptySubst, preludeEnv) stmts
  pure envOut
  where
    step (sAcc, eAcc) st = do
      (s, e) <- inferStmt (apply sAcc eAcc) st
      pure (compose s sAcc, e)

inferExprInEmpty :: Expr -> Either TypeError IType
inferExprInEmpty e = runInfer $ do
  (s, t) <- inferExpr preludeEnv e
  pure (apply s t)

preludeEnv :: TypeEnv
preludeEnv =
  M.fromList
    [ ("print", (Forall [0] (TFunT [TV 0] TNullT), Immutable)),
      ("toString", (Forall [1] (TFunT [TV 1] TStringT), Immutable))
    ]
