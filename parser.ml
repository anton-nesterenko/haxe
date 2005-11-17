(*
 *  Haxe Compiler
 *  Copyright (c)2005 Nicolas Cannasse
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)
open Ast

type error_msg =
	| Unexpected of token
	| Duplicate_default
	| Missing_semicolon

exception Error of error_msg * pos

let error_msg = function
	| Unexpected t -> "Unexpected "^(s_token t)
	| Duplicate_default -> "Duplicate default"
	| Missing_semicolon -> "Missing ;"

let error m p = raise (Error (m,p))

let serror() = raise (Stream.Error "")

let last = ref (Eof,null_pos)

let priority = function
	| OpAssign | OpAssignOp _ -> -4
	| OpBoolOr -> -3
	| OpBoolAnd -> -2
	| OpInterval -> -2
	| OpEq | OpNotEq | OpGt | OpLt | OpGte | OpLte | OpPhysEq | OpPhysNotEq -> -1
	| OpOr | OpAnd | OpXor -> 0
	| OpShl | OpShr | OpUShr -> 1
	| OpAdd | OpSub -> 2
	| OpMod -> 3
	| OpMult | OpDiv -> 4

let is_not_assign = function
	| OpAssign | OpAssignOp _ -> false
	| _ -> true

let can_swap _op op =
	let p1 = priority _op in
	let p2 = priority op in
	if p1 < p2 then
		true
	else if p1 = p2 && p1 >= 0 then (* numerical ops are left-assoc *)
		true
	else
		false

let rec make_binop op e ((v,p2) as e2) =
	match v with
	| EBinop (_op,_e,_e2) when can_swap _op op && (is_not_assign _op || is_not_assign op) ->
		let _e = make_binop op e _e in
		EBinop (_op,_e,_e2) , punion (pos _e) (pos _e2)
	| _ ->
		EBinop (op,e,e2) , punion (pos e) (pos e2)

let rec make_unop op ((v,p2) as e) p1 = 
	match v with
	| EBinop (bop,e,e2) -> EBinop (bop, make_unop op e p1 , e2) , (punion p1 p2)
	| _ ->
		EUnop (op,Prefix,e), punion p1 p2

let popt f = parser
	| [< v = f >] -> Some v
	| [< >] -> None

let rec plist f = parser
	| [< v = f; l = plist f >] -> v :: l
	| [< >] -> []

let rec psep sep f = parser
	| [< v = f; s >] ->
		let rec loop = parser
			| [< '(sep2,_) when sep2 = sep; v = f; l = loop >] -> v :: l
			| [< >] -> []
		in
		v :: loop s
	| [< >] -> []

let ident = parser
	| [< '(Const (Ident i),_) >] -> i

let log m s =
	prerr_endline m

let comma = parser
	| [< '(Comma,_) >] -> ()

let semicolon s =
	if fst (!last) = BrClose then
		snd (!last)
	else
		match s with parser
		| [< '(Semicolon,p) >] -> p
		| [< '(_,p) >] -> error Missing_semicolon p

let rec	parse_file = parser
	| [< '(Const (Ident "package"),_); p = parse_package; _ = semicolon; l = plist parse_type_decl; '(Eof,_); >] -> p , l
	| [< l = plist parse_type_decl; '(Eof,_) >] -> [] , l

and parse_type_decl = parser
	| [< '(Kwd Import,p1); t = parse_type_path_normal; _ = semicolon >] -> (EImport (t.tpackage,t.tname), p1)
	| [< '(Kwd Enum,p1); '(Const (Type name),_);  tl = parse_type_params; '(BrOpen,_); l = plist parse_enum; '(BrClose,p2) >] -> (EEnum (name,tl,l), punion p1 p2)
	| [< n , p1 = parse_class_native; '(Const (Type name),_); tl = parse_type_params; hl = psep Comma parse_class_herit; '(BrOpen,_); fl = plist parse_class_field; '(BrClose,p2) >] -> (EClass (name,tl,n @ hl,fl), punion p1 p2)

and parse_package s = psep Dot ident s

and parse_class_native = parser
	| [< '(Kwd Native,_); '(Kwd Class,p1) >] -> [HNative] , p1
	| [< '(Kwd Class,p1) >] -> [] , p1

and parse_type_opt = parser
	| [< '(DblDot,_); t = parse_type_path >] -> Some t
	| [< >] -> None

and parse_type_path = parser
	| [< '(POpen,_); t = parse_type_path; '(PClose,_); s >] -> parse_type_path_next t s
	| [< '(BrOpen,_); l = psep Comma parse_type_anonymous; '(BrClose,_); s >] -> parse_type_path_next (TPAnonymous l) s
	| [< t = parse_type_path_normal; s >] -> parse_type_path_next (TPNormal t) s

and parse_type_path_normal s = parse_type_path1 [] s

and parse_type_path1 pack = parser
	| [< '(Const (Ident name),_); '(Dot,_); t = parse_type_path1 (name :: pack) >] -> t
	| [< '(Const (Type name),_); s >] ->
		let params = (match s with parser
			| [< '(Binop OpLt,_); l = psep Comma parse_type_path; '(Binop OpGt,_) >] -> l
			| [< >] -> []
		) in
		{
			tpackage = List.rev pack;
			tname = name;
			tparams = params
		}

and parse_type_path_next t = parser
	| [< '(Arrow,_); t2 = parse_type_path >] ->
		(match t2 with
		| TPFunction (args,r) ->
			TPFunction (t :: args,r)
		| _ ->
			TPFunction ([t] , t2))
	| [< >] -> t 

and parse_type_anonymous = parser
	| [< '(Const (Ident name),_); '(DblDot,_); t = parse_type_path >] -> (name,t)

and parse_enum = parser
	| [< '(Const (Ident name),p); s >] ->
		match s with parser
		| [< '(POpen,_); l = psep Comma parse_enum_param; '(PClose,_); _ = semicolon; >] -> (name,l,p)
		| [< '(Semicolon,_) >] -> (name,[],p)
		| [< >] -> serror()

and parse_enum_param = parser
	| [< '(Const (Ident name),_); '(DblDot,_); t = parse_type_path >] -> (name,t)

and parse_class_field = parser
	| [< l = parse_cf_rights []; s >] ->
		match s with parser
		| [< '(Kwd Var,p1); '(Const (Ident name),_); '(DblDot,_); t = parse_type_path; s >] -> 
			let e , p2 = (match s with parser
			| [< '(Binop OpAssign,_) when List.mem AStatic l; e = expr; p2 = semicolon >] -> Some e , p2
			| [< '(Semicolon,p2) >] -> None , p2
			| [< >] -> serror()
			) in
			(FVar (name,l,t,e),punion p1 p2)
		| [< '(Kwd Function,p1); name = parse_fun_name; '(POpen,_); al = psep Comma parse_fun_param; '(PClose,_); t = parse_type_opt; e = expr >] ->
			let f = {
				f_args = al;
				f_type = t;
				f_expr = e;
			} in
			(FFun (name,l,f),punion p1 (pos e))
		| [< >] -> if l = [] then raise Stream.Failure else serror()

and parse_cf_rights l = parser
	| [< '(Kwd Static,_) when not(List.mem AStatic l); l = parse_cf_rights (AStatic :: l) >] -> l
	| [< '(Kwd Public,_) when not(List.mem APublic l || List.mem APrivate l); l = parse_cf_rights (APublic :: l) >] -> l
	| [< '(Kwd Private,_) when not(List.mem APublic l || List.mem APrivate l); l = parse_cf_rights (APrivate :: l) >] -> l
	| [< >] -> l

and parse_fun_name = parser
	| [< '(Const (Ident name),_) >] -> name
	| [< '(Kwd New,_) >] -> "new"

and parse_fun_param = parser
	| [< '(Const (Ident name),_); t = parse_type_opt >] -> (name,t)

and parse_type_params = parser
	| [< '(Binop OpLt,_); l = psep Comma parse_type_param; '(Binop OpGt,_) >] -> l
	| [< >] -> []

and parse_type_param = parser
	| [< '(Const (Type name),_); s >] ->
		match s with parser
		| [< '(DblDot,_); l = psep Comma parse_type_path_normal >] -> (name,l)
		| [< >] -> (name,[])

and parse_class_herit = parser
	| [< '(Kwd Extends,_); t = parse_type_path_normal >] -> HExtends t
	| [< '(Kwd Implements,_); t = parse_type_path_normal >] -> HImplements t

and block1 = parser
	| [< '(Const (Ident name),p); s >] ->
		(match s with parser
		| [< '(DblDot,_); e = expr; l = psep Comma parse_obj_decl; _ = popt comma >] -> EObjectDecl ((name,e) :: l)
		| [< e = expr_next (EConst (Ident name),p); _ = semicolon; b = block >] -> EBlock (e :: b))
	| [< b = block >] -> EBlock b

and block s = plist parse_block_elt s

and parse_block_elt = parser
	| [< '(Kwd Var,p1); vl = psep Comma parse_var_decl; p2 = semicolon >] -> (EVars vl,punion p1 p2)
	| [< e = expr; _ = semicolon >] -> e

and parse_obj_decl = parser
	| [< '(Comma,_); '(Const (Ident name),_); '(DblDot,_); e = expr >] -> (name,e)

and parse_var_decl = parser
	| [< '(Const (Ident name),_); t = parse_type_opt; s >] ->
		match s with parser
		| [< '(Binop OpAssign,_); e = expr >] -> (name,t,Some e)
		| [< >] -> (name,t,None)

and expr = parser
	| [< '(BrOpen,p1); e = block1; '(BrClose,p2) >] -> (e,punion p1 p2)
	| [< '(Const c,p); s >] -> expr_next (EConst c,p) s
	| [< '(Kwd This,p); s >] -> expr_next (EConst (Ident "this"),p) s
	| [< '(Kwd Throw,p); s >] -> expr_next (EConst (Ident "throw"),p) s
	| [< '(Kwd New,p1); t = parse_type_path_normal; '(POpen,_); al = psep Comma expr; '(PClose,p2); s >] -> expr_next (ENew (t,al),punion p1 p2) s
	| [< '(POpen,p1); e = expr; '(PClose,p2); s >] -> expr_next (EParenthesis e, punion p1 p2) s
	| [< '(BkOpen,p1); l = psep Comma expr; _ = popt comma; '(BkClose,p2); s >] -> expr_next (EArrayDecl l, punion p1 p2) s
	| [< '(Kwd Function,p1); '(POpen,_); al = psep Comma parse_fun_param; '(PClose,_); t = parse_type_opt; e = expr; s >] -> 
		let f = {
			f_type = t;
			f_args = al;
			f_expr = e;
		} in
		expr_next (EFunction f, punion p1 (pos e)) s
	| [< '(Unop op,p1) when is_prefix op; e = expr >] -> make_unop op e p1
	| [< '(Binop OpSub,p1); e = expr >] -> make_unop Neg e p1
	| [< '(Kwd For,p); '(Const (Ident name),_); '(Kwd In,_); it = expr; e = expr; s >] -> expr_next (EFor (name,it,e),punion p (pos e)) s
	| [< '(Kwd If,p); cond = expr; e1 = expr; s >] ->
		let e2 , s = (match s with parser
			| [< '(Kwd Else,_); e2 = expr; s >] -> Some e2 , s
			| [< >] -> None , s
		) in
		expr_next (EIf (cond,e1,e2), punion p (match e2 with None -> pos e1 | Some e -> pos e)) s
	| [< '(Kwd Return,p); e = popt expr >] -> (EReturn e, match e with None -> p | Some e -> punion p (pos e))
	| [< '(Kwd Break,p) >] -> (EBreak,p)
	| [< '(Kwd Continue,p) >] -> (EContinue,p)
	| [< '(Kwd While,p1); cond = expr; e = expr; s >] -> expr_next (EWhile (cond,e,NormalWhile),punion p1 (pos e)) s
	| [< '(Kwd Do,p1); e = expr; '(Kwd While,_); cond = expr; s >] -> expr_next (EWhile (cond,e,DoWhile),punion p1 (pos e)) s
	| [< '(Kwd Switch,p1); e = expr; '(BrOpen,_); cases , def = parse_switch_cases; '(BrClose,p2); s >] -> expr_next (ESwitch (e,cases,def),punion p1 p2) s
	| [< '(Kwd Try,p1); e = expr; cl = plist parse_catch; s >] -> expr_next (ETry (e,cl),p1) s
	| [< '(IntInterval i,p1); e2 = expr >] -> make_binop OpInterval (EConst (Int i),p1) e2

and expr_next e1 = parser
	| [< '(Dot,_); s >] -> 
		(match s with parser
		| [< '(Const (Ident f),p); s >] -> expr_next (EField (e1,f) , punion (pos e1) p) s
		| [< '(Const (Type t),p); s >] -> expr_next (EType (e1,t) , punion (pos e1) p) s
		| [< >] -> serror())
	| [< '(POpen,p1); params = psep Comma expr; '(PClose,p2); s >] ->
		expr_next (ECall (e1,params) , punion (pos e1) p2) s
	| [< '(BkOpen,_); e2 = expr; '(BkClose,p2); s >] ->
		expr_next (EArray (e1,e2), punion (pos e1) p2) s
	| [< '(Binop OpGt,_); s >] ->
		(match s with parser
		| [< '(Binop OpGt,_); s >] ->
			(match s with parser
			| [< '(Binop OpGt,_); e2 = expr >] -> make_binop OpUShr e1 e2
			| [< e2 = expr >] -> make_binop OpShr e1 e2
			| [< >] -> serror())
		| [< e2 = expr >] ->
			make_binop OpGt e1 e2
		| [< >] -> serror())
	| [< '(Binop op,_); e2 = expr >] ->
		make_binop op e1 e2
	| [< '(Unop op,p) when is_postfix e1 op; s >] ->
		expr_next (EUnop (op,Postfix,e1), punion (pos e1) p) s
	| [< >] -> e1

and parse_switch_cases = parser
	| [< '(Kwd Default,p1); '(DblDot,_); e = block1; l , def = parse_switch_cases >] -> 
		(match def with None -> () | Some (e,p) -> error Duplicate_default p);
		l , Some (e , p1)
	| [< '(Kwd Case,p1); e = expr; '(DblDot,_); b = block1; l , def = parse_switch_cases >] ->
		(e,(b,p1)) :: l , def
	| [< >] ->
		[] , None

and parse_catch = parser
	| [< '(Kwd Catch,_); '(POpen,_); '(Const (Ident name),_); '(DblDot,_); t = parse_type_path; '(PClose,_); e = expr >] -> (name,t,e)

let parse code file =
	let old = Lexer.save() in
	Lexer.init file;
	last := (Eof,null_pos);
	let rec next_token x =
		let tk = Lexer.token code in
		match fst tk with 
		| Comment s | CommentLine s -> 
			next_token x
		| _ ->
			last := tk;
			Some tk
	in
	try
		let l = parse_file (Stream.from next_token) in
		Lexer.restore old;
		l
	with
		| Stream.Error _
		| Stream.Failure -> 
			Lexer.restore old;
			error (Unexpected (fst !last)) (pos !last)
		| e ->
			Lexer.restore old;
			raise e
