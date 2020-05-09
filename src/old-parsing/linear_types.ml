
exception QError
exception UnusedError
exception ContextError
exception OperandTypeMismatch
exception NoRuleApplies
exception FreeVarNeedsVal

type q =
  | Un
  | Lin

type typ =
  | TyBool of q
  | TyNat of q
  | TyFun of q * typ * typ  
  | TyTensor of q * typ * typ
    
type expr =
  | ExBool of q * bool
  | ExNat of q * int
  | ExVar of string
  | ExLoc of int64
  | ExAbs of q * expr * typ * expr
  | ExApp of expr * expr
  | ExIf of expr * expr * expr
  | ExTensor of q * expr * expr
  | ExSplit of expr * expr * expr * expr
  | ExAlloc of expr
  | ExGaf of expr
  | ExSwap of expr * expr
    
module Ctx = Map.Make(String)
type context = typ Ctx.t

module St = Map.Make(Int64)
type store = expr St.t

let rec is_val t =
  match t with
  | ExNat(Un, _) -> true
  | ExBool(Un, _) -> true
  | ExAbs(Un,_,_,_) -> true
  | ExTensor(Un,t1,t2) -> (is_val t1) && (is_val t2)
  | ExLoc(_) -> true
  | _ -> false

let get_q_t (t : typ) : q =
  match t with
  | TyBool q -> q
  | TyNat q -> q
  | TyFun (q,_,_) -> q
  | TyTensor (q,_,_) -> q

let get_q_e (e : expr) : q =
  match e with
  | ExBool(q, b) -> q
  | ExNat(q, n) -> q
  | ExAbs(q,_,_,_) -> q
  | ExTensor(q,_,_) -> q

let contains (qual : q) (qual' : q) : bool =
  not (qual = Un && qual' = Lin)
  
let rec check_containment (qual : q) (t : typ) : bool =
  match t with
  | TyBool qual' -> contains qual qual'
  | TyNat qual' ->  contains qual qual'
  | TyFun (qual', t1, t2) -> (check_containment qual' t1) && (check_containment qual' t2) && (contains qual qual')
  | TyTensor (qual', t1, t2) -> (check_containment qual' t1) && (check_containment qual' t2) && (contains qual qual')  

let check_context (ctx1 : context) (ctx2 : context) : bool = Ctx.equal (=) ctx1 ctx2
 
 let rec typecheck (ctx : context) (e : expr) : (context * typ) =
  match e with    
  | ExBool (q, b) -> (ctx, TyBool q)
  | ExNat (q, n) -> (ctx, TyNat q)
  | ExVar x -> begin
      let ty = Ctx.find x ctx in
      let qual = get_q_t ty in
      match qual with
      | Un -> (ctx, ty)
      | Lin -> let ctx' = Ctx.remove x ctx in ctx', ty
    end

  | ExAbs (q, ExVar s, ty, e) -> begin
      if (check_containment(get_q_t ty) ty) then () else raise QError;
      let ctx' = Ctx.add s ty ctx in
      let ctx'', ty2 = typecheck ctx' e in
      if (check_containment q ty2) then () else raise QError;
      match (get_q_t ty) with
      | Un -> if check_context ctx (Ctx.remove s ctx'')
              then let ctx''' = (Ctx.remove s ctx'') in ctx''', (TyFun (q, ty, ty2))
              else raise ContextError
      | Lin -> let ctx''' = (Ctx.remove s ctx'') in ctx''', (TyFun (q, ty, ty2))
    end
 
 | ExApp (e1, e2) -> begin
      let ctx1, ty1 = typecheck ctx e1 in
      let ctx2, ty2 = typecheck ctx1 e2 in
      match ty1 with
      | TyFun (q, t1, t2) when t1 = ty2 -> ctx2, t2
      | _ -> raise OperandTypeMismatch
   end
  
 | ExIf(cond_e, e1, e2) -> begin
     let cond_ctx, cond_ty = typecheck ctx cond_e in 
     let ctx2, ty1 = typecheck ctx e1 in
     let ctx3, ty2 = typecheck ctx e2 in
     if check_context ctx2 ctx3 then
       if cond_ty = TyBool Un && ty1 = ty2 then ctx2, ty1
       else raise OperandTypeMismatch
     else raise ContextError
   end
 
 | ExTensor (q, e1, e2) -> let ctx1, ty1 = typecheck ctx e1 in
                           let ctx2, ty2 = typecheck ctx1 e2 in
                           if (check_containment q ty1) && (check_containment q ty2)
                           then ctx2, TyTensor (q, ty1, ty2)
                           else raise ContextError

 | ExSplit (e1, ExVar s1, ExVar s2, e2) -> begin
     let ctx1, ty1 = typecheck ctx e1 in
     let xt, yt = 
       match ty1 with
       | TyTensor(q, t1, t2) -> t1, t2
       | _ -> raise OperandTypeMismatch
     in
     let ctx2 = Ctx.add s1 xt ctx1 in
     let ctx2 = Ctx.add s2 yt ctx2 in    
        let ctx3, ty2 = typecheck ctx2 e2 in 
        if (get_q_t xt) = Lin && (Ctx.mem s1 ctx2) then raise UnusedError else
          if (get_q_t yt) = Lin && (Ctx.mem s2 ctx2) then raise UnusedError else
            let ctx4 = Ctx.remove s1 ctx3 in
            let ctx4 = Ctx.remove s1 ctx4 in ctx4, ty2
   end
 
   | ExAlloc(e) -> begin
      let ctx', t = typecheck ctx e in
      match (get_q_t t) with
      | Lin -> raise QError
      | Un -> begin
          match t with
          | TyNat(q) -> ctx', TyNat(Lin)
          | TyBool(q) -> ctx', TyBool(Lin)
          | TyFun(q,t1,t2) -> ctx', TyFun(Lin,t1,t2)
          | TyTensor(q,t1,t2) -> ctx', TyTensor(Lin,t1,t2)
          end
    end
  
  | ExGaf(e) -> begin
      let ctx', t = typecheck ctx e in
      match (get_q_t t) with
      | Un -> raise QError
      | Lin -> begin
          match t with
          | TyNat(q) -> ctx', TyNat(Un)
          | TyBool(q) -> ctx', TyBool(Un)
          | TyFun(q,t1,t2) -> ctx', TyFun(Un,t1,t2)
          | TyTensor(q,t1,t2) -> ctx', TyTensor(Un,t1,t2)
          end
    end

;; 
(* evaluation *)

(* generate fresh locations through a counter *)
let next_l : int64 ref = ref Int64.zero
let fresh_l () : int64 = begin
    next_l := Int64.add (! next_l) Int64.one;
    ! next_l
    end
  
(* "v for x in t" *)
let substitute x v t =
  let rec subst t =
    match t with
    | ExNat(q, n) -> t
    | ExBool(q, b) -> t
    | ExLoc(l) -> t
    | ExVar(y) -> if x = y then v else t
    | ExApp(e1, e2) -> ExApp(subst e1, subst e2)
    | ExAbs(q, ExVar(x'), ty, e') -> ExAbs(q, ExVar(x'), ty,
                                           if x = x' then e' else subst e')
    | ExTensor(q, t1,t2) -> ExTensor(q, subst t1, subst t2)
    | ExIf(e1, e2, e3) -> ExIf(subst e1, subst e2, subst e3)
    | ExAlloc(e) -> ExAlloc(subst e)
    | ExGaf(e) -> ExGaf(subst e)
    | ExSplit(e1,ExVar(x'),ExVar(y'),e2) -> ExSplit(subst e1,ExVar(x'),ExVar(y'),
                                                    if x=x' || x=y' then e2 else subst e2)
  in subst t

let rec eval_term (s : store) (e : expr) : (store * expr) =
  match e with
  | ExGaf(ExLoc(l)) -> let value = (St.find (l) s) in let s' = (St.remove (l) s) in s', value
  | ExGaf(e) -> let s', e' = eval_term s e in s', ExGaf(e')

  | ExTensor(Lin,e1,e2) when is_val e1 && is_val e2 ->
     let l = fresh_l () in let s' = (St.add l (ExTensor(Un,e1,e2)) s) in s', (ExLoc l)
  | ExAbs(Lin,ExVar(y),ty,e) ->
            let l = fresh_l () in let s' = (St.add l (ExAbs(Un,ExVar(y),ty,e)) s) in s', (ExLoc l)
  | ExAlloc(e) when is_val e -> let l = fresh_l () in let s' = St.add l e s in s', (ExLoc l)

  | ExApp(ExLoc(l), v) when is_val v -> begin
      match (St.find l s) with
      | ExAbs(Un,ExVar(y),ty,e) -> let s' = St.remove l s in s', (substitute y v e)
      | _ -> raise NoRuleApplies
    end
  | ExApp(ExAbs(q, ExVar(x), ty, e), v) when is_val v -> s, (substitute x v e)
  | ExApp(v, e2) when is_val v -> let s', e2' = (eval_term s e2) in (s', ExApp(v, e2'))
  | ExApp(e1, e2) -> let s', e1' = (eval_term s e1) in (s', ExApp(e1', e2)) 

  | ExIf(ExBool(Un,b), e2, e3) -> begin
     match b with
     | true -> s, e2
     | false -> s, e3
    end
  | ExIf(e1, e2, e3) -> let s', e1' = (eval_term s e1) in (s', ExIf(e1', e2, e3))

  | ExSplit(ExTensor(q,e1,e2), ExVar(x), ExVar(y), e3) -> s, (substitute y e2 ( substitute x e1 e3))
  | ExSplit(ExLoc(l), ExVar(x), ExVar(y), e3) -> begin
      match (St.find l s) with
      | ExTensor(Un, e1, e2) -> let s' = St.remove l s in s', (substitute y e2 ( substitute x e1 e3))
      | _ -> raise NoRuleApplies
    end
  | ExSplit(e1, x, y, e2) -> let s', e1' = (eval_term s e1) in s', ExSplit(e1', x, y, e2)

  | _ -> raise NoRuleApplies
 

let rec eval (st : store) (e : expr) : expr =
  try let st', e' = (eval_term st e) in (eval st' e')
  with NoRuleApplies -> e
