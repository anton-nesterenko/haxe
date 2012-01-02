(*
 *  Haxe Compiler
 *  Copyright (c)2005-2007 Nicolas Cannasse
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
open Type
open Common

type context_infos = {
	com : Common.context;
}

type context = {
	inf : context_infos;
	ch : out_channel;
	buf : Buffer.t;
	path : path;
	mutable root_ns : string;
	mutable get_sets : (string * bool,string) Hashtbl.t;
	mutable curclass : tclass;
	mutable tabs : string;
	mutable in_value : tvar option;
	mutable in_static : bool;
	mutable handle_break : bool;
	mutable imports : (string,string list list) Hashtbl.t;
	mutable gen_uid : int;
	mutable local_types : t list;
	mutable constructor_block : bool;
}

let protect name =
	match name with
	| _ -> name

let s_path ctx stat path p =
	match path with
	| ([],name) ->
		(match name with
		| "Int" -> "int"
		| "String" -> "string"
		| "Single" -> "float"
		| "Float" -> "double"
		| "Dynamic" -> "dynamic"
		| "Bool" -> "bool"
		| "Enum" -> "object"
		| "Array" -> "List<object>"
		| _ -> ctx.root_ns ^ "." ^ name)
	| (["haxe"],"Int32") when not stat ->
		"int"
	| (pack,name) ->
		let name = protect name in
		let packs = (try Hashtbl.find ctx.imports name with Not_found -> []) in
		if not (List.mem pack packs) then Hashtbl.replace ctx.imports name (pack :: packs);
		Ast.s_type_path (pack,name)

let reserved =
	let h = Hashtbl.create 0 in
	List.iter (fun l -> Hashtbl.add h l ())
	(* these ones are defined in order to prevent recursion in some Std functions *)
	["is";"as";"int";"uint";"byte";"sbyte";"short";"ushort";"long";"ulong";"float";"double";"decimal";"const";"getTimer";"typeof";"parseInt";"parseFloat";
	(* C# keywords which are not haXe ones *)
	"each";"label";"finally";"with";"sealed";"internal";"const";"namespace";
	(* we don't include get+set since they are not 'real' keywords, but they can't be used as method names *)
	];
	h

(* List of default namespaces to be included in every module *)
let default_namespaces = [
	(["System"],"*");
	(["System";"Collections"],"*");
	(["System";"Collections";"Generic"],"*");
	]

(* Get a non-keyword identifier *)
let s_ident n =
	if Hashtbl.mem reserved n then "_" ^ n else n

(* Gets module path, replacing empty path with ctx.root_ns *)
let get_namespace ctx path =
	match path with
	| ([], _) -> ([ctx.root_ns], (snd path)) (* Use the default root path *)
	| _ -> path

(* Gets type path string minus the class, if no ns path, use default ctx.root_ns *)
let cs_s_type_path ctx (p,s) = 
	match p with 
	| [] -> ctx.root_ns 
	| _ -> String.concat "." p	

(* Start writing a module where path is the module path in pkg, name format *)
let init infos path =
	let rec create acc = function
		| [] -> ()
		| d :: l ->
			let dir = String.concat "/" (List.rev (d :: acc)) in
			if not (Sys.file_exists dir) then Unix.mkdir dir 0o755;
			create (d :: acc) l
	in
	let dir = infos.com.file :: fst path in
	create [] dir;
	let ch = open_out (String.concat "/" dir ^ "/" ^ snd path ^ ".cs") in
	let imports = Hashtbl.create 0 in
	Hashtbl.add imports (snd path) [fst path];
	{
		inf = infos;
		tabs = "";
		ch = ch;
		path = path;
		root_ns = "Program";
		buf = Buffer.create (1 lsl 14);
		in_value = None;
		in_static = false;
		handle_break = false;
		imports = imports;
		curclass = null_class;
		gen_uid = 0;
		local_types = [];
		get_sets = Hashtbl.create 0;
		constructor_block = false;
	}
						
let close ctx =
	let ns_path = (get_namespace ctx ctx.path) in
	output_string ctx.ch (Printf.sprintf "namespace %s {\n" (String.concat "." (fst ns_path)));
	List.iter (fun path ->
		output_string ctx.ch ("\tusing " ^ (cs_s_type_path ctx path) ^ ";\n");
	) default_namespaces;
	Hashtbl.iter (fun name paths ->
		List.iter (fun pack ->
			let path = pack, name in
			if path <> ctx.path then output_string ctx.ch ("\tusing " ^ (cs_s_type_path ctx path) ^ ";\n");
		) paths
	) ctx.imports;
	output_string ctx.ch (Buffer.contents ctx.buf);
	close_out ctx.ch

let save_locals ctx =
	(fun() -> ())

let gen_local ctx l =
	ctx.gen_uid <- ctx.gen_uid + 1;
	if ctx.gen_uid = 1 then l else l ^ string_of_int ctx.gen_uid

let spr ctx s = Buffer.add_string ctx.buf s
let print ctx = Printf.kprintf (fun s -> Buffer.add_string ctx.buf s)

let unsupported p = error "This expression cannot be generated to C#" p

let newline ctx =
	let rec loop p =
		let ch = (if p >= 0 then (Buffer.nth ctx.buf p) else ';') in
		match ch with
		| '}' | '{' | ':' | ';' -> print ctx "\n%s" ctx.tabs
		| '\n' | '\t' -> loop (p - 1)
		| _ -> print ctx ";\n%s" ctx.tabs
	in
	loop (Buffer.length ctx.buf - 1)

let rec concat ctx s f = function
	| [] -> ()
	| [x] -> f x
	| x :: l ->
		f x;
		spr ctx s;
		concat ctx s f l

let open_block ctx =
	let oldt = ctx.tabs in
	ctx.tabs <- "\t" ^ ctx.tabs;
	(fun() -> ctx.tabs <- oldt)

let parent e =
	match e.eexpr with
	| TParenthesis _ -> e
	| _ -> mk (TParenthesis e) e.etype e.epos

let default_value tstr =
	match tstr with
	| "int" | "uint" -> "0"
	| "float" | "double" -> "0"
	| "bool" -> "false"
	| _ -> "null"

let rec type_str ctx t p =
	match t with
	| TEnum _ | TInst _ when List.memq t ctx.local_types ->
		"dynamic"
	| TEnum (e,_) ->
		if e.e_extern then (match e.e_path with
			| [], "Void" -> "void"
			| [], "Bool" -> "bool"
			| _ ->
				let rec loop = function
					| [] -> "object"
					| (":fakeEnum",[Ast.EConst (Ast.Type n),_],_) :: _ ->
						(match n with
						| "Int" -> "int"
						| "UInt" -> "uint"
						| _ -> n)
					| _ :: l -> loop l
				in
				loop e.e_meta
		) else
			s_path ctx true e.e_path p
	| TInst (c,_) ->
		(match c.cl_kind with
		| KNormal | KGeneric | KGenericInstance _ -> s_path ctx false c.cl_path p
		| KTypeParameter | KExtension _ | KExpr _ | KMacroType -> "dynamic")
	| TFun _ ->
		"object"
	| TMono r ->
		(match !r with None -> "dynamic" | Some t -> type_str ctx t p)
	| TAnon _ | TDynamic _ ->
		"dynamic"
	| TType (t,args) ->
		(match t.t_path with
		| [], "UInt" -> "uint"
		| [] , "Null" ->
			(match args with
			| [t] ->
				(match follow t with
				| TInst ({ cl_path = [],"Int" },_)
				| TInst ({ cl_path = [],"Float" },_)
				| TEnum ({ e_path = [],"Bool" },_) -> "dynamic"
				| _ -> type_str ctx t p)
			| _ -> assert false);
		| _ -> type_str ctx (apply_params t.t_types args t.t_type) p)
	| TLazy f ->
		type_str ctx ((!f)()) p

let rec iter_switch_break in_switch e =
	match e.eexpr with
	| TFunction _ | TWhile _ | TFor _ -> ()
	| TSwitch _ | TMatch _ when not in_switch -> iter_switch_break true e
	| TBreak when in_switch -> raise Exit
	| _ -> iter (iter_switch_break in_switch) e

let handle_break ctx e =
	let old_handle = ctx.handle_break in
	try
		iter_switch_break false e;
		ctx.handle_break <- false;
		(fun() -> ctx.handle_break <- old_handle)
	with
		Exit ->
			spr ctx "try {";
			let b = open_block ctx in
			newline ctx;
			ctx.handle_break <- true;
			(fun() ->
				b();
				ctx.handle_break <- old_handle;
				newline ctx;
				spr ctx "} catch( Exception e ) { if( e != \"__break__\" ) throw new e; }";
			)

let this ctx = if ctx.in_value <> None then "$this" else "this"

let escape_bin s =
	let b = Buffer.create 0 in
	for i = 0 to String.length s - 1 do
		match Char.code (String.unsafe_get s i) with
		| c when c < 32 -> Buffer.add_string b (Printf.sprintf "\\x%.2X" c)
		| c -> Buffer.add_char b (Char.chr c)
	done;
	Buffer.contents b

let gen_constant ctx p = function
	| TInt i -> print ctx "%ld" i
	| TFloat s -> spr ctx s
	| TString s -> print ctx "\"%s\"" (escape_bin (Ast.s_escape s))
	| TBool b -> spr ctx (if b then "true" else "false")
	| TNull -> spr ctx "null"
	| TThis -> spr ctx (this ctx)
	| TSuper -> spr ctx "base"

let gen_function_header ctx name f params p =
	let old = ctx.in_value in
	let locals = save_locals ctx in
	let old_t = ctx.local_types in
	ctx.in_value <- None;
	ctx.local_types <- List.map snd params @ ctx.local_types;
	print ctx "%s" (type_str ctx f.tf_type p);
	print ctx "%s(" (match name with None -> "" | Some (n,meta) ->
		let rec loop = function
			| [] -> n
			| (":getter",[Ast.EConst (Ast.Ident i | Ast.Type i),_],_) :: _ -> "get " ^ i
			| (":setter",[Ast.EConst (Ast.Ident i | Ast.Type i),_],_) :: _ -> "set " ^ i
			| _ :: l -> loop l
		in
		" " ^ loop meta
	);
	concat ctx "," (fun (v,c) ->
		let tstr = type_str ctx v.v_type p in
		print ctx "%s %s" tstr (s_ident v.v_name);
		match c with
		| None ->
			if ctx.constructor_block then print ctx " = %s" (default_value tstr);
		| Some c ->
			spr ctx " = ";
			gen_constant ctx p c
	) f.tf_args;
	print ctx ")";
	(fun () ->
		ctx.in_value <- old;
		locals();
		ctx.local_types <- old_t;
	)

let rec gen_call ctx e el r =
	match e.eexpr , el with
	| TCall (x,_) , el ->
		spr ctx "(";
		gen_value ctx e;
		spr ctx ")";
		spr ctx "(";
		concat ctx "," (gen_value ctx) el;
		spr ctx ")";
	| TLocal { v_name = "__is__" } , [e1;e2] ->
		gen_value ctx e1;
		spr ctx " is ";
		gen_value ctx e2;
	| TLocal { v_name = "__as__" }, [e1;e2] ->
		gen_value ctx e1;
		spr ctx " as ";
		gen_value ctx e2;
	| TLocal { v_name = "__int__" }, [e] ->
		spr ctx "(int)(";
		gen_value ctx e;
		spr ctx ")";
	| TLocal { v_name = "__float__" }, [e] ->
		spr ctx "(float)(";
		gen_value ctx e;
		spr ctx ")";
	| TLocal { v_name = "__double__" }, [e] ->
		spr ctx "(double)(";
		gen_value ctx e;
		spr ctx ")";
	| TLocal { v_name = "__typeof__" }, [e] ->
		spr ctx "typeof(";
		gen_value ctx e;
		spr ctx ")";
	| TLocal { v_name = "__keys__" }, [e] ->
		let ret = (match ctx.in_value with None -> assert false | Some r -> r) in
		print ctx "%s = new List<object>()" ret.v_name;
		newline ctx;
		let b = save_locals ctx in
		let tmp = gen_local ctx "$k" in
		print ctx "foreach (string %s in " tmp;
		gen_value ctx e;
		print ctx ") %s.push(%s)" ret.v_name tmp;
		b();
	| TLocal { v_name = "__hkeys__" }, [e] ->
		let ret = (match ctx.in_value with None -> assert false | Some r -> r) in
		print ctx "%s = new List<object>" ret.v_name;
		newline ctx;
		let b = save_locals ctx in
		let tmp = gen_local ctx "$k" in
		print ctx "foreach (string %s in " tmp;
		gen_value ctx e;
		print ctx ") %s.Add(%s.substr(1))" ret.v_name tmp;
		b();
	| TLocal { v_name = "__foreach__" }, [e] ->
		let ret = (match ctx.in_value with None -> assert false | Some r -> r) in
		print ctx "%s = new List<object>()" ret.v_name;
		newline ctx;
		let b = save_locals ctx in
		let tmp = gen_local ctx "$k" in
		print ctx "foreach (object %s in " tmp;
		gen_value ctx e;
		print ctx ") %s.push(%s)" ret.v_name tmp;
		b();
	| TLocal { v_name = "__new__" }, e :: args ->
		spr ctx "new ";
		gen_value ctx e;
		spr ctx "(";
		concat ctx "," (gen_value ctx) args;
		spr ctx ")";
	| TLocal { v_name = "__delete__" }, [e;f] ->
		spr ctx "delete(";
		gen_value ctx e;
		spr ctx "[";
		gen_value ctx f;
		spr ctx "]";
		spr ctx ")";
	| TLocal { v_name = "__unprotect__" }, [e] ->
		gen_value ctx e
	| TLocal { v_name = "__vector__" }, [e] ->
		spr ctx (type_str ctx r e.epos);
		spr ctx "(";
		gen_value ctx e;
		spr ctx ")"
	| _ ->
		gen_value ctx e;
		spr ctx "(";
		concat ctx "," (gen_value ctx) el;
		spr ctx ")"

and gen_value_op ctx e =
	match e.eexpr with
	| TBinop (op,_,_) when op = Ast.OpAnd || op = Ast.OpOr || op = Ast.OpXor ->
		spr ctx "(";
		gen_value ctx e;
		spr ctx ")";
	| _ ->
		gen_value ctx e

and gen_field_access ctx t s =
	let field c =
		match fst c.cl_path, snd c.cl_path, s with
		| [], "Math", "NaN"
		| [], "Math", "NEGATIVE_INFINITY"
		| [], "Math", "POSITIVE_INFINITY"
		| [], "Math", "isFinite"
		| [], "Math", "isNaN"
		| [], "Date", "now"
		| [], "Date", "fromTime"
		| [], "Date", "fromString"
		| [], "String", "charCodeAt"
		->
			print ctx "[\"%s\"]" s
		| [], "Date", "toString" ->
			print ctx "[\"toStringHX\"]"
		| [], "String", "cca" ->
			print ctx ".charCodeAt"
		| ["flash";"xml"], "XML", "namespace" ->
			print ctx ".namespace"
		| _ ->
			print ctx ".%s" (s_ident s)
	in
	match follow t with
	| TInst (c,_) -> field c
	| TAnon a ->
		(match !(a.a_status) with
		| Statics c -> field c
		| _ -> print ctx ".%s" (s_ident s))
	| _ ->
		print ctx ".%s" (s_ident s)

and gen_expr ctx e =
	match e.eexpr with
	| TConst c ->
		gen_constant ctx e.epos c
	| TLocal v ->
		spr ctx (s_ident v.v_name)
	| TEnumField (en,s) ->
		print ctx "%s.%s" (s_path ctx true en.e_path e.epos) (s_ident s)
	| TArray ({ eexpr = TLocal { v_name = "__global__" } },{ eexpr = TConst (TString s) }) ->
		let path = Ast.parse_path s in
		spr ctx (s_path ctx false path e.epos)
	| TArray (e1,e2) ->
		gen_value ctx e1;
		spr ctx "[";
		gen_value ctx e2;
		spr ctx "]";
	| TBinop (op,{ eexpr = TField (e1,s) },e2) ->
		gen_value_op ctx e1;
		gen_field_access ctx e1.etype s;
		print ctx " %s " (Ast.s_binop op);
		gen_value_op ctx e2;
	| TBinop (op,e1,e2) ->
		gen_value_op ctx e1;
		print ctx " %s " (Ast.s_binop op);
		gen_value_op ctx e2;
	| TField (e,s) | TClosure (e,s) ->
   		gen_value ctx e;
		gen_field_access ctx e.etype s
	| TTypeExpr t ->
		spr ctx (s_path ctx true (t_path t) e.epos)
	| TParenthesis e ->
		spr ctx "(";
		gen_value ctx e;
		spr ctx ")";
	| TReturn eo ->
		if ctx.in_value <> None then unsupported e.epos;
		(match eo with
		| None ->
			spr ctx "return"
		| Some e when (match follow e.etype with TEnum({ e_path = [],"Void" },[]) -> true | _ -> false) ->
			gen_value ctx e;
			newline ctx;
			spr ctx "return"
		| Some e ->
			spr ctx "return ";
			gen_value ctx e);
	| TBreak ->
		if ctx.in_value <> None then unsupported e.epos;
		if ctx.handle_break then spr ctx "throw \"__break__\"" else spr ctx "break"
	| TContinue ->
		if ctx.in_value <> None then unsupported e.epos;
		spr ctx "continue"
	| TBlock el ->
		let b = save_locals ctx in
		print ctx "{";
		let bend = open_block ctx in
		let cb = (if not ctx.constructor_block then
			(fun () -> ())
		else if not (Codegen.constructor_side_effects e) then begin
			ctx.constructor_block <- false;
			(fun () -> ())
		end else begin
			ctx.constructor_block <- false;
			print ctx " if( !%s.skip_constructor ) {" (s_path ctx true (["cs"],"Boot") e.epos);
            (fun() -> print ctx "}")
		end) in
		List.iter (fun e -> newline ctx; gen_expr ctx e) el;
		bend();
		newline ctx;
		cb();
		print ctx "}";
		b();
	| TFunction f ->
		let h = gen_function_header ctx None f [] e.epos in
		let old = ctx.in_static in
		ctx.in_static <- true;
		gen_expr ctx f.tf_expr;
		ctx.in_static <- old;
		h();
	| TCall (v,el) ->
		gen_call ctx v el e.etype
	| TArrayDecl el ->
		spr ctx "[";
		concat ctx "," (gen_value ctx) el;
		spr ctx "]"
	| TThrow e ->
		spr ctx "throw ";
		gen_value ctx e;
	| TVars [] ->
		()
	| TVars vl ->
		concat ctx ", " (fun (v,eo) ->
			print ctx "%s %s" (type_str ctx v.v_type e.epos) (s_ident v.v_name);
			match eo with
			| None -> ()
			| Some e ->
				spr ctx " = ";
				gen_value ctx e
		) vl;
	| TNew (c,params,el) ->
		(match c.cl_path, params with
		| (["flash"],"Vector"), [pt] -> print ctx "new Vector.<%s>(" (type_str ctx pt e.epos)
		| _ -> print ctx "new %s(" (s_path ctx true c.cl_path e.epos));
		concat ctx "," (gen_value ctx) el;
		spr ctx ")"
	| TIf (cond,e,eelse) ->
		spr ctx "if";
		gen_value ctx (parent cond);
		spr ctx " ";
		gen_expr ctx e;
		(match eelse with
		| None -> ()
		| Some e ->
			newline ctx;
			spr ctx "else ";
			gen_expr ctx e);
	| TUnop (op,Ast.Prefix,e) ->
		spr ctx (Ast.s_unop op);
		gen_value ctx e
	| TUnop (op,Ast.Postfix,e) ->
		gen_value ctx e;
		spr ctx (Ast.s_unop op)
	| TWhile (cond,e,Ast.NormalWhile) ->
		let handle_break = handle_break ctx e in
		spr ctx "while";
		gen_value ctx (parent cond);
		spr ctx " ";
		gen_expr ctx e;
		handle_break();
	| TWhile (cond,e,Ast.DoWhile) ->
		let handle_break = handle_break ctx e in
		spr ctx "do ";
		gen_expr ctx e;
		spr ctx " while";
		gen_value ctx (parent cond);
		handle_break();
	| TObjectDecl fields ->
		spr ctx "{ ";
		concat ctx ", " (fun (f,e) -> print ctx "%s : " (s_ident f); gen_value ctx e) fields;
		spr ctx "}"
	| TFor (v,it,e) ->
		let handle_break = handle_break ctx e in
		let b = save_locals ctx in
		let tmp = gen_local ctx "$it" in
		print ctx "{ dynamic %s = " tmp;
		gen_value ctx it;
		newline ctx;
		print ctx "while( %s.hasNext() ) { %s %s = %s.next()" tmp (type_str ctx v.v_type e.epos) (s_ident v.v_name) tmp;
		newline ctx;
		gen_expr ctx e;
		newline ctx;
		spr ctx "}}";
		b();
		handle_break();
	| TTry (e,catchs) ->
		spr ctx "try ";
		gen_expr ctx e;
		List.iter (fun (v,e) ->
			newline ctx;
			print ctx "catch( %s %s )" (type_str ctx v.v_type e.epos) (s_ident v.v_name);
			gen_expr ctx e;
		) catchs;
	| TMatch (e,_,cases,def) ->
		print ctx "{";
		let bend = open_block ctx in
		newline ctx;
		let b = save_locals ctx in
		let tmp = gen_local ctx "$e" in
		print ctx "HaxeEnum %s = " tmp;
		gen_value ctx e;
		newline ctx;
		print ctx "switch( %s.index ) {" tmp;
		List.iter (fun (cl,params,e) ->
			List.iter (fun c ->
				newline ctx;
				print ctx "case %d:" c;
			) cl;
			let b = save_locals ctx in
			(match params with
			| None | Some [] -> ()
			| Some l ->
				let n = ref (-1) in
				let l = List.fold_left (fun acc v -> incr n; match v with None -> acc | Some v -> (v,!n) :: acc) [] l in
				match l with
				| [] -> ()
				| l ->
					newline ctx;
					concat ctx ", " (fun (v,n) ->
						print ctx "%s %s = %s.__params[%d]" (type_str ctx v.v_type e.epos) (s_ident v.v_name) tmp n;
					) l);
			gen_block ctx e;
			print ctx "break";
			b()
		) cases;
		(match def with
		| None -> ()
		| Some e ->
			newline ctx;
			spr ctx "default:";
			gen_block ctx e;
			print ctx "break";
		);
		newline ctx;
		spr ctx "}";
		bend();
		newline ctx;
		spr ctx "}";
		b()
	| TSwitch (e,cases,def) ->
		spr ctx "switch";
		gen_value ctx (parent e);
		spr ctx " {";
		newline ctx;
		List.iter (fun (el,e2) ->
			List.iter (fun e ->
				spr ctx "case ";
				gen_value ctx e;
				spr ctx ":";
			) el;
			gen_block ctx e2;
			print ctx "break";
			newline ctx;
		) cases;
		(match def with
		| None -> ()
		| Some e ->
			spr ctx "default:";
			gen_block ctx e;
			print ctx "break";
			newline ctx;
		);
		spr ctx "}"
	| TCast (e1,None) ->
		spr ctx "((";
		gen_expr ctx e1;
		print ctx ") as %s)" (type_str ctx e.etype e.epos);
	| TCast (e1,Some t) ->
		gen_expr ctx (Codegen.default_cast ctx.inf.com e1 t e.etype e.epos)

and gen_block ctx e =
	newline ctx;
	match e.eexpr with
	| TBlock [] -> ()
	| _ -> 
		gen_expr ctx e;
		newline ctx

and gen_value ctx e =
	let assign e =
		mk (TBinop (Ast.OpAssign,
			mk (TLocal (match ctx.in_value with None -> assert false | Some r -> r)) t_dynamic e.epos,
			e
		)) e.etype e.epos
	in
	let block e =
		mk (TBlock [e]) e.etype e.epos
	in
	let value block =
		let old = ctx.in_value in
		let t = type_str ctx e.etype e.epos in
		let locs = save_locals ctx in
		let r = alloc_var (gen_local ctx "$r") e.etype in
		ctx.in_value <- Some r;
		if ctx.in_static then
			print ctx "function() : %s " t
		else
			print ctx "(function($this:%s) : %s " (snd ctx.path) t;
		let b = if block then begin
			spr ctx "{";
			let b = open_block ctx in
			newline ctx;
			print ctx "%s %s" t r.v_name;
			newline ctx;
			b
		end else
			(fun() -> ())
		in
		(fun() ->
			if block then begin
				newline ctx;
				print ctx "return %s" r.v_name;
				b();
				newline ctx;
				spr ctx "}";
			end;
			ctx.in_value <- old;
			locs();
			if ctx.in_static then
				print ctx "()"
			else
				print ctx "(%s))" (this ctx)
		)
	in
	match e.eexpr with
	| TCall ({ eexpr = TLocal { v_name = "__keys__" } },_) | TCall ({ eexpr = TLocal { v_name = "__hkeys__" } },_) ->
		let v = value true in
		gen_expr ctx e;
		v()
	| TConst _
	| TLocal _
	| TEnumField _
	| TArray _
	| TBinop _
	| TField _
	| TClosure _
	| TTypeExpr _
	| TParenthesis _
	| TObjectDecl _
	| TArrayDecl _
	| TCall _
	| TNew _
	| TUnop _
	| TFunction _ ->
		gen_expr ctx e
	| TCast (e1,t) ->
		gen_value ctx (match t with None -> e1 | Some t -> Codegen.default_cast ctx.inf.com e1 t e.etype e.epos)
	| TReturn _
	| TBreak
	| TContinue ->
		unsupported e.epos
	| TVars _
	| TFor _
	| TWhile _
	| TThrow _ ->
		(* value is discarded anyway *)
		let v = value true in
		gen_expr ctx e;
		v()
	| TBlock [] ->
		spr ctx "null"
	| TBlock [e] ->
		gen_value ctx e
	| TBlock el ->
		let v = value true in
		let rec loop = function
			| [] ->
				spr ctx "return null";
			| [e] ->
				gen_expr ctx (assign e);
			| e :: l ->
				gen_expr ctx e;
				newline ctx;
				loop l
		in
		loop el;
		v();
	| TIf (cond,e,eo) ->
		spr ctx "(";
		gen_value ctx cond;
		spr ctx "?";
		gen_value ctx e;
		spr ctx ":";
		(match eo with
		| None -> spr ctx "null"
		| Some e -> gen_value ctx e);
		spr ctx ")"
	| TSwitch (cond,cases,def) ->
		let v = value true in
		gen_expr ctx (mk (TSwitch (cond,
			List.map (fun (e1,e2) -> (e1,assign e2)) cases,
			match def with None -> None | Some e -> Some (assign e)
		)) e.etype e.epos);
		v()
	| TMatch (cond,enum,cases,def) ->
		let v = value true in
		gen_expr ctx (mk (TMatch (cond,enum,
			List.map (fun (constr,params,e) -> (constr,params,assign e)) cases,
			match def with None -> None | Some e -> Some (assign e)
		)) e.etype e.epos);
		v()
	| TTry (b,catchs) ->
		let v = value true in
		gen_expr ctx (mk (TTry (block (assign b),
			List.map (fun (v,e) -> v, block (assign e)) catchs
		)) e.etype e.epos);
		v()

let generate_field ctx static f =
	newline ctx;
	ctx.in_static <- static;
	ctx.gen_uid <- 0;
	List.iter (fun(m,pl,_) ->
		match m,pl with
		| ":meta", [Ast.ECall ((Ast.EConst (Ast.Ident n | Ast.Type n),_),args),_] ->
			let mk_arg (a,p) =
				match a with
				| Ast.EConst (Ast.String s) -> (None, s)
				| Ast.EBinop (Ast.OpAssign,(Ast.EConst (Ast.Ident n | Ast.Type n),_),(Ast.EConst (Ast.String s),_)) -> (Some n, s)
				| _ -> error "Invalid meta definition" p
			in
			print ctx "[%s" n;
			(match args with
			| [] -> ()
			| _ ->
				print ctx "(";
				concat ctx "," (fun a ->
					match mk_arg a with
					| None, s -> gen_constant ctx (snd a) (TString s)
					| Some s, e -> print ctx "%s=" s; gen_constant ctx (snd a) (TString e)
				) args;
				print ctx ")");
			print ctx "]";
		| _ -> ()
	) f.cf_meta;
	let public = f.cf_public || Hashtbl.mem ctx.get_sets (f.cf_name,static) || (f.cf_name = "main" && static) || f.cf_name = "resolve" in
	let rights = (if static then "static " else "") ^ (if public then "public" else "protected") in
	let p = ctx.curclass.cl_pos in
	match f.cf_expr, f.cf_kind with
	| Some { eexpr = TFunction fd }, Method (MethNormal | MethInline) ->
		print ctx "%s " rights;
		let rec loop c =
			match c.cl_super with
			| None -> ()
			| Some (c,_) ->
				if PMap.mem f.cf_name c.cl_fields then
					spr ctx "override "
				else
					loop c
		in
		if not static then loop ctx.curclass;
		let h = gen_function_header ctx (Some (s_ident f.cf_name, f.cf_meta)) fd f.cf_params p in
		gen_expr ctx fd.tf_expr;
		h();
		newline ctx
	| _ ->
		let is_get = (match f.cf_kind with Var { v_read = AccCall _ } -> true | _ -> false) in
		let is_set = (match f.cf_kind with Var { v_write = AccCall _ } -> true | _ -> false) in
		let is_getset = (is_get || is_set) in
		if ctx.curclass.cl_interface then
			match follow f.cf_type with
			| TFun (args,r) ->
				print ctx "%s %s(" (type_str ctx r p) f.cf_name;
				concat ctx "," (fun (arg,o,t) ->
					let tstr = type_str ctx t p in
					print ctx "%s %s" tstr arg;
					if o then print ctx " = %s" (default_value tstr);
				) args;
				print ctx ")";
			| _ when is_getset ->
				let t = type_str ctx f.cf_type p in
				let id = s_ident f.cf_name in
				(match f.cf_kind with
				| Var v -> print ctx "%s %s {%s%s };" t id (if is_get then " get;" else "") (if is_set then " set;" else "");
				| _ -> assert false)
			| _ -> ()
		else
		if is_getset then begin
			print ctx "%s %s %s {" rights (type_str ctx f.cf_type p) (s_ident f.cf_name);
			newline ctx;
			let b = open_block ctx in
			let id = s_ident f.cf_name in
			let v = (match f.cf_kind with Var v -> v | _ -> assert false) in
			(match v.v_read with
			| AccNormal | AccNo | AccNever ->
				print ctx "get { return __%s; }" id;
			| AccCall m ->
				print ctx "get { return %s(); }" m;
			| _ ->
				());
			(match v.v_write with
			| AccNormal ->
				print ctx "set { __%s = value; }" id;
			| AccCall m ->
				print ctx "set { %s(value); }" m;
			| AccNo | AccNever ->
				print ctx "%s set { __%s = value; }" (if v.v_write = AccNo then "protected" else "private") id;
			| _ -> ());
			b();
			newline ctx;
			print ctx "}";
		end else begin
			print ctx "%s %s %s" rights (type_str ctx f.cf_type p) (s_ident f.cf_name);
			match f.cf_expr with
			| None -> ()
			| Some e ->
				print ctx " = ";
				gen_value ctx e
		end

let rec define_getset ctx stat c =
	let def f name =
		Hashtbl.add ctx.get_sets (name,stat) f.cf_name
	in
	let field f =
		match f.cf_kind with
		| Method _ -> ()
		| Var v ->
			(match v.v_read with AccCall m -> def f m | _ -> ());
			(match v.v_write with AccCall m -> def f m | _ -> ())
	in
	List.iter field (if stat then c.cl_ordered_statics else c.cl_ordered_fields);
	match c.cl_super with
	| Some (c,_) when not stat -> define_getset ctx stat c
	| _ -> ()


let generate_class ctx c =
	ctx.curclass <- c;
	define_getset ctx true c;
	define_getset ctx false c;
	ctx.local_types <- List.map snd c.cl_types;
	let pack = open_block ctx in
	print ctx "\tpublic %s%s %s" (match c.cl_dynamic with None -> "" | Some _ -> if c.cl_interface then "" else "dynamic ") (if c.cl_interface then "interface" else "class") (snd c.cl_path);
	(match c.cl_super, c.cl_implements with
	| None, [] -> ()
	| _ -> print ctx " : ");
	(match c.cl_super with
	| None -> ()
	| Some (csup,_) -> print ctx "%s " (s_path ctx true csup.cl_path c.cl_pos));
	(match c.cl_implements with
	| [] -> ()
	| l ->
		(match c.cl_super with
		| None -> ()
		| Some (_,_) -> print ctx ", ");
		concat ctx ", " (fun (i,_) -> print ctx "%s" (s_path ctx true i.cl_path c.cl_pos)) l);
	spr ctx "{";
	let cl = open_block ctx in
	(match c.cl_constructor with
	| None -> ()
	| Some f ->
		let f = { f with
			cf_name = snd c.cl_path;
			cf_public = true;
			cf_kind = Method MethNormal;
		} in
		ctx.constructor_block <- true;
		generate_field ctx false f;
	);
	List.iter (generate_field ctx false) c.cl_ordered_fields;
	List.iter (generate_field ctx true) c.cl_ordered_statics;
	cl();
	newline ctx;
	print ctx "}";
	pack();
	newline ctx;
	print ctx "}";
	newline ctx

let generate_main ctx inits =
	ctx.curclass <- { null_class with cl_path = [],"__main__" };
	newline ctx;
	let pack = open_block ctx in
	print ctx "\tpublic class __main__ : %s {" (s_path ctx true (["cs"],"Boot") Ast.null_pos);
	let cl = open_block ctx in
	newline ctx;
	spr ctx "public static void Main() {";
	let fl = open_block ctx in
	newline ctx;
	print ctx "%s.current = new __main__()" (s_path ctx true (["cs"],"Lib") Ast.null_pos);
	List.iter (fun e -> newline ctx; gen_expr ctx e) inits;
	fl();
	newline ctx;
	print ctx "}";
	cl();
	newline ctx;
	print ctx "}";
	pack();
	newline ctx;
	print ctx "}";
	newline ctx

let generate_enum ctx e =
	ctx.local_types <- List.map snd e.e_types;
	let pack = open_block ctx in
	let ename = snd e.e_path in
	print ctx "\tpublic class %s : HaxeEnum {" ename;
	let cl = open_block ctx in
	newline ctx;
	print ctx "public static bool __isenum = true";
	newline ctx;
	print ctx "public %s(string t, int i, List<object> p = null) { this.__tag = t; this.__index = i; this.__params = p; }" ename;
	PMap.iter (fun _ c ->
		newline ctx;
		match c.ef_type with
		| TFun (args,_) ->
			print ctx "public static %s %s(" ename c.ef_name;
			concat ctx ", " (fun (a,o,t) ->
				print ctx "%s %s" (type_str ctx t c.ef_pos) (s_ident a) ;
				if o then spr ctx " = null";
			) args;
			print ctx ") {";
			print ctx " return new %s(\"%s\",%d," ename c.ef_name c.ef_index;
			concat ctx "," (fun (a,_,_) -> spr ctx (s_ident a)) args;
			print ctx "); }";
		| _ ->
			print ctx "public static %s %s = new %s(\"%s\",%d)" ename c.ef_name ename c.ef_name c.ef_index;
	) e.e_constrs;
	newline ctx;
	(match Codegen.build_metadata ctx.inf.com (TEnumDecl e) with
	| None -> ()
	| Some e ->
		print ctx "public static object __meta__ = ";
		gen_expr ctx e;
		newline ctx);
	print ctx "public static List<object> __constructs__ = new List<object> { %s };" (String.concat "," (List.map (fun s -> "\"" ^ Ast.s_escape s ^ "\"") e.e_names));
	cl();
	newline ctx;
	print ctx "}";
	pack();
	newline ctx;
	print ctx "}";
	newline ctx

let generate_base_enum ctx =
	let pack = open_block ctx in
	spr ctx "\tpublic class HaxeEnum {";
	let cl = open_block ctx in
	newline ctx;
	spr ctx "public string __tag";
	newline ctx;
	spr ctx "public int __index";
	newline ctx;
	spr ctx "public List<object> __params";
	cl();
	newline ctx;
	print ctx "}";
	pack();
	newline ctx;
	print ctx "}";
	newline ctx

let generate com =
	let infos = {
		com = com;
	} in
	let ctx = init infos ([],"HaxeEnum") in
	generate_base_enum ctx;
	close ctx;
	let inits = ref [] in
	List.iter (fun t ->
		match t with
		| TClassDecl c ->
			let c = (match c.cl_path with
				| ["cs"],"CSharpXml__" -> { c with cl_path = [],"Xml" }
				| (pack,name) -> { c with cl_path = (pack,protect name) }
			) in
			(match c.cl_init with
			| None -> ()
			| Some e -> inits := e :: !inits);
			if c.cl_extern then
				()
			else
				let ctx = init infos c.cl_path in
				generate_class ctx c;
				close ctx
		| TEnumDecl e ->
			let pack,name = e.e_path in
			let e = { e with e_path = (pack,protect name) } in
			if e.e_extern && e.e_path <> ([],"Void") then
				()
			else
				let ctx = init infos e.e_path in
				generate_enum ctx e;
				close ctx
		| TTypeDecl t ->
			()
	) com.types;
	(match com.main with
	| None -> ()
	| Some e -> inits := e :: !inits);
	let ctx = init infos ([],"__main__") in
	generate_main ctx (List.rev !inits);
	close ctx
