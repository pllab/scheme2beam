
exception NoRuleApplies			      
exception FreeVarNeedsVal
exception OperandTypeMismatch
exception NoSuchVar

(* type variable = string *)

(* todo ATTAPL gives Bool,Pair,Fun as all being qualified un/lin... *)
type typ =
  | TyNat
  (* | TyPair of typ * typ *)
  | TyFun of typ * typ
  (* | TyBool *)
  (* | TyLinNat  *)
  | TyTensor of typ * typ
  | TyLolli of typ * typ

type term =
  | TConst of int
  | TVar of string             (* free var *)
  | TAbs of term * typ * term  (* binding var; apparently this can be (un)restricted? *)
  | TApp of term * term
  | TPlus of term * term
  (* linear operands *)
  | Tun of term                (* qualifier unrestricted *)
  | Tlin of term               (* qualifier restricted *)
  | TTensor of term * term
  | TLolli of term * term

type context = (string * typ) list

let rec contains l x =
  match l with
  | [] -> false
  | (h1,h2)::t -> if h1 = x then true else contains t x
			      
let replace l x y = List.map (fun (e1,e2) -> if e1=x then (e1,y) else (e1,e2)) l
			     
let extend l x y = 
  match (contains l x) with
  | true -> replace l x y
  | false -> (x,y)::l

let rec lookup_var ctx (v: string) : typ =
  match ctx with
  | [] -> raise NoSuchVar
  | (v',t)::xs -> if v = v' then t else lookup_var xs v
	    
let rec is_val t =
  match t with
  | TConst(_) -> true
  | TAbs(_,_,_) -> true
  | _ -> false

let substitute x v t =
  let rec subst t =
    match t with
    | TConst(_) -> t
    | TVar(y) -> if x = y then v else t
    | TApp(t1,t2) -> TApp(subst t1, subst t2)
    | TAbs(TVar x', ty, t') -> TAbs(TVar x', ty,if x = x' then t' else subst t')
    | TPlus(t1,t2) -> TPlus(subst t1, subst t2)
  in subst t
				
let rec eval_term t =
  match t with    
  | TVar(x) -> raise (FreeVarNeedsVal)		     		     
  | TPlus(TConst(t1), TConst(t2)) -> TConst(t1+t2)
  | TPlus(TConst(t1), t2) -> let t2' = eval_term t2 in TPlus(TConst(t1), t2')
  | TPlus(t1, t2) -> let t1' = eval_term t1 in TPlus(t1', t2)
  | TApp(TAbs(TVar(x),ty,t),v) when is_val v -> substitute x v t
  | TApp(v,t) when is_val v -> let t' = eval_term t in TApp(v,t')
  | TApp(t1,t2) -> let t1' = eval_term t1 in TApp(t1',t2)
  | _ -> raise NoRuleApplies 						 

let rec typeof ctx t =
  match t with
  | TConst(t) when t >= 0 -> TyNat
  | TVar(v) -> lookup_var ctx v
  | TPlus(t1,t2) -> if typeof ctx t1 = TyNat && typeof ctx t2 = TyNat
  		    then typeof ctx t2 else raise OperandTypeMismatch
  | TAbs(TVar x, ty, t') -> let ctx' = extend ctx x ty in
  			    let typ2 = typeof ctx' t' in
  			    TyFun(ty, typ2)
  | TApp(t1, t2) -> let typ1 = typeof ctx t1 in
  		    let typ2 = typeof ctx t2 in
  		    match typ1 with
  		    | TyFun(typ1', typ2') when typ1' = typ2 -> typ2'
  		    | _ -> raise OperandTypeMismatch
	       
let test_to_string a =
  match a with
  | true -> "[TEST PASSED]\n"
  | false -> "[TEST FAILED]\n"

let typ_to_string a =
  match a with
  | TyNat -> "TyNat"
  | TyFun(f1,f2) -> "TyFun"

let rec eval t =
  try let t' = eval_term t
      in eval t'
  with NoRuleApplies -> t

