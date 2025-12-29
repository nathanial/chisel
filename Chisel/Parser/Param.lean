/-
  Chisel.Parser.Param
  Parameter binding API for safe SQL parameterization
-/
import Chisel.Core.DML
import Chisel.Core.DDL

namespace Chisel.Parser

/-- Information about parameters in a parsed query -/
structure ParamInfo where
  positionalCount : Nat := 0
  indexedParams : List Nat := []
  namedParams : List String := []
  deriving Repr, Inhabited

/-- Binding error -/
inductive BindError where
  | missingPositional (index : Nat)
  | missingNamed (name : String)
  | missingIndexed (index : Nat)
  | typeMismatch (expected found : String)
  deriving Repr

instance : ToString BindError where
  toString
    | .missingPositional i => s!"Missing positional parameter at index {i}"
    | .missingNamed n => s!"Missing named parameter: {n}"
    | .missingIndexed i => s!"Missing indexed parameter: ${i}"
    | .typeMismatch e f => s!"Type mismatch: expected {e}, found {f}"

/-- Merge two ParamInfo -/
def mergeParamInfo (a b : ParamInfo) : ParamInfo :=
  { positionalCount := a.positionalCount + b.positionalCount
    indexedParams := a.indexedParams ++ b.indexedParams
    namedParams := a.namedParams ++ b.namedParams }

mutual

/-- Collect parameter information from an expression -/
partial def collectExprParams (expr : Expr) : ParamInfo :=
  match expr with
  | .param name index =>
    match name, index with
    | some n, _ => { namedParams := [n] }
    | none, some i => { indexedParams := [i] }
    | none, none => { positionalCount := 1 }
  | .binary _ l r =>
    mergeParamInfo (collectExprParams l) (collectExprParams r)
  | .unary _ e => collectExprParams e
  | .between e l u =>
    mergeParamInfo (collectExprParams e) (mergeParamInfo (collectExprParams l) (collectExprParams u))
  | .inValues e vs =>
    vs.foldl (fun acc v => mergeParamInfo acc (collectExprParams v)) (collectExprParams e)
  | .notInValues e vs =>
    vs.foldl (fun acc v => mergeParamInfo acc (collectExprParams v)) (collectExprParams e)
  | .inSubquery e s =>
    mergeParamInfo (collectExprParams e) (collectSelectParams s)
  | .notInSubquery e s =>
    mergeParamInfo (collectExprParams e) (collectSelectParams s)
  | .exists_ s => collectSelectParams s
  | .notExists s => collectSelectParams s
  | .case_ cases else_ =>
    let caseInfo := cases.foldl (fun acc (c, r) =>
      mergeParamInfo acc (mergeParamInfo (collectExprParams c) (collectExprParams r))) {}
    match else_ with
    | some e => mergeParamInfo caseInfo (collectExprParams e)
    | none => caseInfo
  | .cast e _ => collectExprParams e
  | .func _ args =>
    args.foldl (fun acc a => mergeParamInfo acc (collectExprParams a)) {}
  | .agg _ e _ =>
    match e with
    | some expr => collectExprParams expr
    | none => {}
  | .subquery s => collectSelectParams s
  | _ => {}

/-- Collect parameter information from a SELECT statement -/
partial def collectSelectParams (s : SelectCore) : ParamInfo :=
  let colInfo := s.columns.foldl (fun acc c => mergeParamInfo acc (collectExprParams c.expr)) {}
  let fromInfo := match s.from_ with
    | some t => collectTableRefParams t
    | none => {}
  let whereInfo := match s.where_ with
    | some e => collectExprParams e
    | none => {}
  let groupInfo := s.groupBy.foldl (fun acc e => mergeParamInfo acc (collectExprParams e)) {}
  let havingInfo := match s.having with
    | some e => collectExprParams e
    | none => {}
  let orderInfo := s.orderBy.foldl (fun acc o => mergeParamInfo acc (collectExprParams o.expr)) {}
  [colInfo, fromInfo, whereInfo, groupInfo, havingInfo, orderInfo].foldl mergeParamInfo {}

/-- Collect parameter information from a table reference -/
partial def collectTableRefParams (t : TableRef) : ParamInfo :=
  match t with
  | .table _ _ => {}
  | .join _ l r on =>
    let joinInfo := mergeParamInfo (collectTableRefParams l) (collectTableRefParams r)
    match on with
    | some e => mergeParamInfo joinInfo (collectExprParams e)
    | none => joinInfo
  | .subquery s _ => collectSelectParams s

end

/-- Bind positional parameters (?) in an expression -/
partial def bindPositional (expr : Expr) (values : List Literal) : Except BindError Expr :=
  bindPositionalState expr values 0 |>.map Prod.fst
where
  bindPositionalState (e : Expr) (vs : List Literal) (idx : Nat) : Except BindError (Expr × Nat) :=
    match e with
    | .param none none =>
      match vs[idx]? with
      | some v => .ok (.lit v, idx + 1)
      | none => .error (.missingPositional idx)
    | .binary op l r => do
      let (l', idx') ← bindPositionalState l vs idx
      let (r', idx'') ← bindPositionalState r vs idx'
      .ok (.binary op l' r', idx'')
    | .unary op e => do
      let (e', idx') ← bindPositionalState e vs idx
      .ok (.unary op e', idx')
    | .between e l u => do
      let (e', i1) ← bindPositionalState e vs idx
      let (l', i2) ← bindPositionalState l vs i1
      let (u', i3) ← bindPositionalState u vs i2
      .ok (.between e' l' u', i3)
    | .inValues e vals => do
      let (e', i1) ← bindPositionalState e vs idx
      let (vals', i2) ← bindListState vals vs i1
      .ok (.inValues e' vals', i2)
    | .notInValues e vals => do
      let (e', i1) ← bindPositionalState e vs idx
      let (vals', i2) ← bindListState vals vs i1
      .ok (.notInValues e' vals', i2)
    | .case_ cases else_ => do
      let (cases', i1) ← bindCasesState cases vs idx
      match else_ with
      | some el => do
        let (el', i2) ← bindPositionalState el vs i1
        .ok (.case_ cases' (some el'), i2)
      | none => .ok (.case_ cases' none, i1)
    | .cast e t => do
      let (e', idx') ← bindPositionalState e vs idx
      .ok (.cast e' t, idx')
    | .func n args => do
      let (args', idx') ← bindListState args vs idx
      .ok (.func n args', idx')
    | .agg f e d =>
      match e with
      | some expr => do
        let (expr', idx') ← bindPositionalState expr vs idx
        .ok (.agg f (some expr') d, idx')
      | none => .ok (.agg f none d, idx)
    | other => .ok (other, idx)

  bindListState (es : List Expr) (vs : List Literal) (idx : Nat) : Except BindError (List Expr × Nat) :=
    match es with
    | [] => .ok ([], idx)
    | e :: rest => do
      let (e', idx') ← bindPositionalState e vs idx
      let (rest', idx'') ← bindListState rest vs idx'
      .ok (e' :: rest', idx'')

  bindCasesState (cs : List (Expr × Expr)) (vs : List Literal) (idx : Nat) : Except BindError (List (Expr × Expr) × Nat) :=
    match cs with
    | [] => .ok ([], idx)
    | (c, r) :: rest => do
      let (c', i1) ← bindPositionalState c vs idx
      let (r', i2) ← bindPositionalState r vs i1
      let (rest', i3) ← bindCasesState rest vs i2
      .ok ((c', r') :: rest', i3)

/-- Bind named parameters (:name, @name) in an expression -/
partial def bindNamed (expr : Expr) (values : List (String × Literal)) : Except BindError Expr :=
  match expr with
  | .param (some name) _ =>
    match values.find? (·.fst == name) with
    | some (_, v) => .ok (.lit v)
    | none => .error (.missingNamed name)
  | .binary op l r => do
    let l' ← bindNamed l values
    let r' ← bindNamed r values
    .ok (.binary op l' r')
  | .unary op e => do
    let e' ← bindNamed e values
    .ok (.unary op e')
  | .between e l u => do
    let e' ← bindNamed e values
    let l' ← bindNamed l values
    let u' ← bindNamed u values
    .ok (.between e' l' u')
  | .inValues e vals => do
    let e' ← bindNamed e values
    let vals' ← vals.mapM (bindNamed · values)
    .ok (.inValues e' vals')
  | .notInValues e vals => do
    let e' ← bindNamed e values
    let vals' ← vals.mapM (bindNamed · values)
    .ok (.notInValues e' vals')
  | .case_ cases else_ => do
    let cases' ← cases.mapM fun (c, r) => do
      let c' ← bindNamed c values
      let r' ← bindNamed r values
      .ok (c', r')
    let else_' ← match else_ with
      | some e => some <$> bindNamed e values
      | none => .ok none
    .ok (.case_ cases' else_')
  | .cast e t => do
    let e' ← bindNamed e values
    .ok (.cast e' t)
  | .func n args => do
    let args' ← args.mapM (bindNamed · values)
    .ok (.func n args')
  | .agg f e d =>
    match e with
    | some expr => do
      let expr' ← bindNamed expr values
      .ok (.agg f (some expr') d)
    | none => .ok (.agg f none d)
  | other => .ok other

/-- Bind indexed parameters ($1, $2, etc.) in an expression -/
partial def bindIndexed (expr : Expr) (values : Array Literal) : Except BindError Expr :=
  match expr with
  | .param none (some idx) =>
    match values[idx - 1]? with  -- $1 is index 0
    | some v => .ok (.lit v)
    | none => .error (.missingIndexed idx)
  | .binary op l r => do
    let l' ← bindIndexed l values
    let r' ← bindIndexed r values
    .ok (.binary op l' r')
  | .unary op e => do
    let e' ← bindIndexed e values
    .ok (.unary op e')
  | .between e l u => do
    let e' ← bindIndexed e values
    let l' ← bindIndexed l values
    let u' ← bindIndexed u values
    .ok (.between e' l' u')
  | .inValues e vals => do
    let e' ← bindIndexed e values
    let vals' ← vals.mapM (bindIndexed · values)
    .ok (.inValues e' vals')
  | .notInValues e vals => do
    let e' ← bindIndexed e values
    let vals' ← vals.mapM (bindIndexed · values)
    .ok (.notInValues e' vals')
  | .case_ cases else_ => do
    let cases' ← cases.mapM fun (c, r) => do
      let c' ← bindIndexed c values
      let r' ← bindIndexed r values
      .ok (c', r')
    let else_' ← match else_ with
      | some e => some <$> bindIndexed e values
      | none => .ok none
    .ok (.case_ cases' else_')
  | .cast e t => do
    let e' ← bindIndexed e values
    .ok (.cast e' t)
  | .func n args => do
    let args' ← args.mapM (bindIndexed · values)
    .ok (.func n args')
  | .agg f e d =>
    match e with
    | some expr => do
      let expr' ← bindIndexed expr values
      .ok (.agg f (some expr') d)
    | none => .ok (.agg f none d)
  | other => .ok other

end Chisel.Parser
