(*
 *  Haxe Compiler
 *  Copyright (c)2006 Nicolas Cannasse
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
open Type
open As3
open As3hl
open Common

type read = Read
type write = Unused__ | Write

type tkind =
	| KInt
	| KUInt
	| KFloat
	| KBool
	| KType of hl_name
	| KDynamic
	| KNone

type register = {
	rid : int;
	rtype : tkind;
	mutable rused : bool;
	mutable rinit : bool;
	mutable rcond : bool;
}

type 'a access =
	| VReg of register
	| VId of hl_name
	| VCast of hl_name * tkind
	| VGlobal of hl_name
	| VArray
	| VScope of hl_slot
	| VVolatile of hl_name * tkind option

type local =
	| LReg of register
	| LScope of hl_slot
	| LGlobal of hl_name

type code_infos = {
	mutable iregs : register DynArray.t;
	mutable ipos : int;
	mutable istack : int;
	mutable imax : int;
	mutable iscopes : int;
	mutable imaxscopes : int;
	mutable iloop : int;
	mutable icond : bool;
}

type try_infos = {
	tr_pos : int;
	tr_end : int;
	tr_catch_pos : int;
	tr_type : t;
}

type context = {
	(* globals *)
	com : Common.context;
	debugger : bool;
	swc : bool;
	boot : path;
	swf_protected : bool;
	mutable debug : bool;
	mutable last_line : int;
	mutable last_file : string;
	(* per-function *)
	mutable locals : (int,tvar * local) PMap.t;
	mutable code : hl_opcode DynArray.t;
	mutable infos : code_infos;
	mutable trys : try_infos list;
	mutable breaks : (unit -> unit) list;
	mutable continues : (int -> unit) list;
	mutable in_static : bool;
	mutable block_vars : (hl_slot * string * hl_name option) list;
	mutable used_vars : (string , pos) PMap.t;
	mutable try_scope_reg : register option;
	mutable for_call : bool;
}

let invalid_expr p = error "Invalid expression" p
let stack_error p = error "Stack error" p

let index_int (x : int) : 'a index = Obj.magic (x + 1)
let index_nz_int (x : int) : 'a index_nz = Obj.magic x
let tid (x : 'a index) : int = Obj.magic x

let ethis = mk (TConst TThis) (mk_mono()) null_pos
let dynamic_prop = HMMultiNameLate [HNPublic (Some "")]

let write ctx op =
	DynArray.add ctx.code op;
	ctx.infos.ipos <- ctx.infos.ipos + 1;
	let s = ctx.infos.istack + As3hlparse.stack_delta op in
	ctx.infos.istack <- s;
	if s > ctx.infos.imax then ctx.infos.imax <- s;
	match op with
	| HScope ->
		let n = ctx.infos.iscopes + 1 in
		ctx.infos.iscopes <- n;
		if n > ctx.infos.imaxscopes then ctx.infos.imaxscopes <- n
	| HPopScope ->
		ctx.infos.iscopes <- ctx.infos.iscopes - 1
	| _ ->
		()

let jump ctx cond =
	let op = DynArray.length ctx.code in
	let p = ctx.infos.ipos in
	write ctx (HJump (cond,0));
	(fun () ->
		let delta = ctx.infos.ipos - p in
		DynArray.set ctx.code op (HJump (cond,delta))
	)

let jump_back ctx =
	let p = ctx.infos.ipos in
	write ctx HLabel;
	(fun cond ->
		let delta = p - ctx.infos.ipos in
		write ctx (HJump (cond,delta))
	)

let real_path = function
	| [] , "Int" -> [] , "int"
	| [] , "UInt" -> [] , "uint"
	| [] , "Float" -> [] , "Number"
	| [] , "Bool" -> [] , "Boolean"
	| [] , "Enum" -> [] , "Class"
	| ["flash";"xml"], "XML" -> [], "XML"
	| ["flash";"xml"], "XMLList" -> [], "XMLList"
	| ["flash";"utils"], "QName" -> [] , "QName"
	| ["flash";"utils"], "Namespace" -> [] , "Namespace"
	| ["flash"] , "FlashXml__" -> [] , "Xml"
	| ["flash";"errors"] , "Error" -> [], "Error"
	| ["flash"] , "Vector" -> ["__AS3__";"vec"], "Vector"
	| path -> path

let type_path ctx path =
	let pack, name = real_path path in
	HMPath (pack,name)

let rec follow_basic t =
	match t with
	| TMono r ->
		(match !r with
		| Some t -> follow_basic t
		| _ -> t)
	| TLazy f ->
		follow_basic (!f())
	| TType ({ t_path = [],"Null" },[tp]) ->
		(match follow_basic tp with
		| TMono _
		| TFun _
		| TInst ({ cl_path = (["haxe"],"Int32") },[])
		| TInst ({ cl_path = ([],"Int") },[])
		| TInst ({ cl_path = ([],"Float") },[])
		| TType ({ t_path = [],"UInt" },[])
		| TEnum ({ e_path = ([],"Bool") },[]) -> t
		| t -> t)
	| TType ({ t_path = [],"UInt" },[]) ->
		t
	| TType (t,tl) ->
		follow_basic (apply_params t.t_types tl t.t_type)
	| _ -> t

let rec type_id ctx t =
	match follow_basic t with
	| TInst ({ cl_path = ["haxe"],"Int32" },_) ->
		type_path ctx ([],"Int")
	| TInst ({ cl_path = ["flash"],"Vector" } as c,pl) ->
		HMParams (type_path ctx c.cl_path,List.map (type_id ctx) pl)
	| TInst (c,_) ->
		(match c.cl_kind with
		| KTypeParameter ->
			(match c.cl_implements with
			| [csup,_] -> type_path ctx csup.cl_path
			| _ -> type_path ctx ([],"Object"))
		| KExtension (c,params) ->
			type_id ctx (TInst (c,params))
		| _ ->
			type_path ctx c.cl_path)
	| TFun _ ->
		type_path ctx ([],"Function")
	| TType ({ t_path = ([],"UInt") as path },_) ->
		type_path ctx path
	| TEnum ({ e_path = [],"XmlType"; e_extern = true },_) ->
		HMPath ([],"String")
	| TEnum (e,_) ->
		let rec loop = function
			| [] -> type_path ctx e.e_path
			| (":fakeEnum",[Ast.EConst (Ast.Type n),_],_) :: _ -> type_path ctx ([],n)
			| _ :: l -> loop l
		in
		loop e.e_meta
	| _ ->
		HMPath ([],"Object")

let type_opt ctx t =
	match follow t with
	| TDynamic _ | TMono _ -> None
	| _ -> Some (type_id ctx t)

let type_void ctx t =
	match follow t with
	| TEnum ({ e_path = [],"Void" },_) -> Some (HMPath ([],"void"))
	| _ -> type_opt ctx t

let classify ctx t =
	match follow_basic t with
	| TInst ({ cl_path = [],"Int" },_) | TInst ({ cl_path = ["haxe"],"Int32" },_) ->
		KInt
	| TInst ({ cl_path = [],"Float" },_) ->
		KFloat
	| TEnum ({ e_path = [],"Bool" },_) ->
		KBool
	| TEnum ({ e_path = [],"XmlType"; e_extern = true },_) ->
		KType (HMPath ([],"String"))
	| TEnum (e,_) ->
		let rec loop = function
			| [] -> KType (type_id ctx t)
			| (":fakeEnum",[Ast.EConst (Type n),_],_) :: _ ->
				(match n with
				| "Int" -> KInt
				| "UInt" -> KUInt
				| "String" -> KType (HMPath ([],"String"))
				| _ -> assert false)
			| _ :: l -> loop l
		in
		loop e.e_meta
	| TInst _ ->
		KType (type_id ctx t)
	| TType ({ t_path = [],"UInt" },_) ->
		KUInt
	| TFun _ ->
		KType (HMPath ([],"Function"))
	| TAnon a ->
		(match !(a.a_status) with
		| Statics _ -> KNone
		| _ -> KDynamic)
	| TMono _
	| TType _
	| TDynamic _ ->
		KDynamic
	| TLazy _ ->
		assert false

let ident i =
	(* some field identifiers might cause issues with SWC *)
	match i with
	| "int" -> HMPath ([],"_" ^ i)
	| _ -> HMPath ([],i)

let as3 p =
	HMName (p,HNNamespace "http://adobe.com/AS3/2006/builtin")

let property ctx p t =
	match follow t with
	| TInst ({ cl_path = [],"Array" },_) ->
		(match p with
		| "length" -> ident p, Some KInt, false (* UInt in the spec *)
		| "copy" | "insert" | "remove" | "iterator" | "toString" -> ident p , None, true
		| _ -> as3 p, None, false);
	| TInst ({ cl_path = ["flash"],"Vector" },_) ->
		(match p with
		| "length" | "fixed" | "toString" -> ident p, None, false
		| "iterator" -> ident p, None, true
		| _ -> as3 p, None, false);
	| TInst ({ cl_path = [],"String" },_) ->
		(match p with
		| "length" (* Int in AS3/haXe *) -> ident p, None, false
		| "charCodeAt" (* use haXe version *) -> ident p, None, true
		| "cca" -> as3 "charCodeAt", None, false
		| _ -> as3 p, None, false);
	| TAnon a ->
		(match !(a.a_status) with
		| Statics { cl_path = [], "Math" } ->
			(match p with
			| "POSITIVE_INFINITY" | "NEGATIVE_INFINITY" | "NaN" -> ident p, Some KFloat, false
			| _ -> ident p, None, false)
		| _ -> ident p, None, false)
	| TInst ({ cl_kind = KExtension _ } as c,params) ->
		(* cast type when accessing an extension field *)
		(try
			let f = PMap.find p c.cl_fields in
			ident p, Some (classify ctx (apply_params c.cl_types params f.cf_type)), false
		with Not_found ->
			ident p, None, false)
	| _ ->
		ident p, None, false

let default_infos() =
	{
		ipos = 0;
		istack = 0;
		imax = 0;
		iregs = DynArray.create();
		iscopes = 0;
		imaxscopes = 0;
		iloop = -1;
		icond = false;
	}

let alloc_reg ctx k =
	let regs = ctx.infos.iregs in
	try
		let p = DynArray.index_of (fun r -> not r.rused && k = r.rtype) regs in
		let r = DynArray.unsafe_get regs p in
		r.rused <- true;
		r.rinit <- false;
		r
	with
		Not_found ->
			let r = {
				rid = DynArray.length regs + 1;
				rused = true;
				rinit = false;
				rtype = k;
				rcond = false;
			} in
			DynArray.add regs r;
			r

let coerce ctx t =
	(* it would be useful to know if we don't already have
	   this type on the stack (as detected by the bytecode verifier)...
	   maybe this get removed at JIT, so it's only useful to reduce codesize
	*)
	if t <> KNone then
	write ctx (match t with
		| KInt -> HToInt
		| KUInt -> HToUInt
		| KFloat -> HToNumber
		| KBool -> HToBool
		| KType t -> HCast t
		| KDynamic -> HAsAny
		| KNone -> assert false
	)

let set_reg ctx r =
	if not r.rinit then begin
		r.rinit <- true;
		if ctx.infos.icond then r.rcond <- true;
	end;
	coerce ctx r.rtype;
	write ctx (HSetReg r.rid)

let set_reg_dup ctx r =
	if not r.rinit then begin
		r.rinit <- true;
		if ctx.infos.icond then r.rcond <- true;
	end;
	coerce ctx r.rtype;
	write ctx HDup;
	write ctx (HSetReg r.rid)

let free_reg ctx r =
	r.rused <- false

let pop ctx n =
	let rec loop n =
		if n > 0 then begin
			write ctx HPop;
			loop (n - 1)
		end
	in
	if n < 0 then assert false;
	let old = ctx.infos.istack in
	loop n;
	ctx.infos.istack <- old

let define_local ctx ?(init=false) v p =
	let name = v.v_name in
	let t = v.v_type in
	let l = (if v.v_capture then begin
			let topt = type_opt ctx t in
			let pos = (try
				let slot , _ , t = (List.find (fun (_,x,_) -> name = x) ctx.block_vars) in
				if t <> topt then error ("Local variable '" ^ name ^ "' captured with same name but different types") p;
				slot
			with
				Not_found ->
					let n = List.length ctx.block_vars + 1 in
					ctx.block_vars <- (n,name,topt) :: ctx.block_vars;
					n
			) in
			LScope pos
		end else
			let r = alloc_reg ctx (classify ctx t) in
			if ctx.debug then write ctx (HDebugReg (name, r.rid, ctx.last_line));
			r.rinit <- init;
			LReg r
	) in
	ctx.locals <- PMap.add v.v_id (v,l) ctx.locals

let is_set v = (Obj.magic v) = Write

let gen_local_access ctx v p (forset : 'a)  : 'a access =
	match snd (try PMap.find v.v_id ctx.locals with Not_found -> error ("Unbound variable " ^ v.v_name) p) with
	| LReg r ->
		VReg r
	| LScope n ->
		write ctx (HGetScope 1);
		VScope n
	| LGlobal p ->
		if is_set forset then write ctx (HFindProp p);
		VGlobal p

let get_local_register ctx v =
	match (try snd (PMap.find v.v_id ctx.locals) with Not_found -> LScope 0) with
	| LReg r -> Some r
	| _ -> None

let rec setvar ctx (acc : write access) kret =
	match acc with
	| VReg r ->
		if kret <> None then
			set_reg_dup ctx r
		else
			set_reg ctx r;
	| VGlobal _ | VId _ | VCast _ | VArray | VScope _ when kret <> None ->
		let r = alloc_reg ctx (match kret with None -> assert false | Some k -> k) in
		set_reg_dup ctx r;
		setvar ctx acc None;
		write ctx (HReg r.rid);
		free_reg ctx r
	| VGlobal g ->
		write ctx (HSetProp g)
	| VId id | VCast (id,_) ->
		write ctx (HInitProp id)
	| VVolatile (id,_) ->
		write ctx (HArray 1);
		write ctx (HInitProp id)
	| VArray ->
		write ctx (HSetProp dynamic_prop);
		ctx.infos.istack <- ctx.infos.istack - 1
	| VScope n ->
		write ctx (HSetSlot n)

let getvar ctx (acc : read access) =
	match acc with
	| VReg r ->
		if not r.rinit then begin
			r.rinit <- true;
			r.rcond <- true;
		end;
		write ctx (HReg r.rid)
	| VId id ->
		write ctx (HGetProp id)
	| VVolatile (id,t) ->
		write ctx (HGetProp id);
		write ctx (HSmallInt 0);
		write ctx (HGetProp dynamic_prop);
		ctx.infos.istack <- ctx.infos.istack - 1;
		(match t with
		| None -> ()
		| Some t -> coerce ctx t)
	| VCast (id,t) ->
		write ctx (HGetProp id);
		coerce ctx t
	| VGlobal g ->
		write ctx (HGetLex g);
	| VArray ->
		write ctx (HGetProp dynamic_prop);
		ctx.infos.istack <- ctx.infos.istack - 1
	| VScope n ->
		write ctx (HGetSlot n)

let open_block ctx retval =
	let old_stack = ctx.infos.istack in
	let old_regs = DynArray.map (fun r -> r.rused) ctx.infos.iregs in
	let old_locals = ctx.locals in
	(fun() ->
		if ctx.infos.istack <> old_stack + (if retval then 1 else 0) then assert false;
		let rcount = DynArray.length old_regs + 1 in
		DynArray.iter (fun r ->
			if r.rid < rcount then
				r.rused <- DynArray.unsafe_get old_regs (r.rid - 1)
			else
				r.rused <- false
		) ctx.infos.iregs;
		ctx.locals <- old_locals;
	)

let begin_branch ctx =
	if ctx.infos.icond then
		(fun() -> ())
	else begin
		ctx.infos.icond <- true;
		(fun() -> ctx.infos.icond <- false)
	end

let begin_switch ctx =
	let branch = begin_branch ctx in
	let switch_index = DynArray.length ctx.code in
	let switch_pos = ctx.infos.ipos in
	write ctx (HSwitch (0,[]));
	let constructs = ref [] in
	let max = ref 0 in
	let ftag tag =
		if tag > !max then max := tag;
		constructs := (tag,ctx.infos.ipos) :: !constructs;
	in
	let fend() =
		let cases = Array.create (!max + 1) 1 in
		List.iter (fun (tag,pos) -> Array.set cases tag (pos - switch_pos)) !constructs;
		DynArray.set ctx.code switch_index (HSwitch (1,Array.to_list cases));
		branch();
	in
	fend, ftag


let debug_infos ?(is_min=true) ctx p =
	if ctx.debug then begin
		let line = Lexer.get_error_line (if is_min then p else { p with pmin = p.pmax }) in
		if ctx.last_file <> p.pfile then begin
			write ctx (HDebugFile (if ctx.debugger then Common.get_full_path p.pfile else p.pfile));
			ctx.last_file <- p.pfile;
			ctx.last_line <- -1;
		end;
		if ctx.last_line <> line then begin
			write ctx (HDebugLine line);
			ctx.last_line <- line;
		end
	end

let gen_constant ctx c t p =
	match c with
	| TInt i ->
		let unsigned = classify ctx t = KUInt in
		if Int32.compare i (-128l) > 0 && Int32.compare i 128l < 0 then begin
			write ctx (HSmallInt (Int32.to_int i));
			if unsigned then write ctx HToUInt;
		end else
			write ctx (if unsigned then HUIntRef i else HIntRef i)
	| TFloat f ->
		let f = float_of_string f in
		write ctx (HFloat f);
	| TString s ->
		write ctx (HString (Genswf8.to_utf8 s));
	| TBool b ->
		write ctx (if b then HTrue else HFalse);
	| TNull ->
		write ctx HNull;
		(match classify ctx t with
		| KInt | KBool | KUInt | KFloat ->
			error ("In Flash9, null can't be used as basic type " ^ s_type (print_context()) t) p
		| x -> coerce ctx x)
	| TThis ->
		write ctx HThis
	| TSuper ->
		assert false

let end_fun ctx args dparams tret =
	{
		hlmt_index = 0;
		hlmt_ret = type_void ctx tret;
		hlmt_args = List.map (fun (v,_) -> type_opt ctx v.v_type) args;
		hlmt_native = false;
		hlmt_var_args = false;
		hlmt_debug_name = None;
		hlmt_dparams = dparams;
		hlmt_pnames = if ctx.swc || ctx.debugger then Some (List.map (fun (v,_) -> Some v.v_name) args) else None;
		hlmt_new_block = false;
		hlmt_unused_flag = false;
		hlmt_arguments_defined = false;
		hlmt_uses_dxns = false;
		hlmt_function = None;
	}

let begin_fun ctx args tret el stat p =
	let old_locals = ctx.locals in
	let old_code = ctx.code in
	let old_infos = ctx.infos in
	let old_trys = ctx.trys in
	let old_bvars = ctx.block_vars in
	let old_static = ctx.in_static in
	let last_line = ctx.last_line in
	let old_treg = ctx.try_scope_reg in
	let old_uvars = ctx.used_vars in
	ctx.infos <- default_infos();
	ctx.code <- DynArray.create();
	ctx.used_vars <- PMap.empty;
	ctx.trys <- [];
	ctx.block_vars <- [];
	ctx.in_static <- stat;
	ctx.last_line <- -1;
	ctx.last_file <- "";
	debug_infos ctx p;
	let rec find_this e =
		match e.eexpr with
		| TFunction _ -> ()
		| TConst TThis | TConst TSuper -> raise Exit
		| _ -> Type.iter find_this e
	in
	let this_reg = try List.iter find_this el; false with Exit -> true in
	ctx.locals <- PMap.foldi (fun _ (v,l) acc ->
		match l with
		| LReg _ -> acc
		| LScope _ -> PMap.add v.v_id (v,LGlobal (ident v.v_name)) acc
		| LGlobal _ -> PMap.add v.v_id (v,l) acc
	) ctx.locals PMap.empty;

	let dparams = ref None in
	let make_constant_value r c t =
		let v = (match classify ctx t, c with
		| _, None -> HVNone
		| (KInt | KFloat | KUInt | KBool) as kind, Some c ->
			(match c with
			| TInt i -> if kind = KUInt then HVUInt i else HVInt i
			| TFloat s -> HVFloat (float_of_string s)
			| TBool b -> HVBool b
			| TNull -> error ("In Flash9, null can't be used as basic type " ^ s_type (print_context()) t) p
			| _ -> assert false)
		| _, Some TNull -> HVNone
		| k, Some c ->
			write ctx (HReg r.rid);
			write ctx HNull;
			let j = jump ctx J3Neq in
			gen_constant ctx c t p;
			coerce ctx k;
			write ctx (HSetReg r.rid);
			j();
			HVNone
		) in
		match !dparams with
		| None -> if c <> None then dparams := Some [v]
		| Some l -> dparams := Some (v :: l)
	in

	List.iter (fun (v,c) ->
		let t = v.v_type in
		define_local ctx v ~init:true p;
		match gen_local_access ctx v null_pos Write with
		| VReg r ->
			make_constant_value r c t
		| acc ->
			let r = alloc_reg ctx (classify ctx t) in
			make_constant_value r c t;
			write ctx (HReg r.rid);
			setvar ctx acc None
	) args;

	let dparams = (match !dparams with None -> None | Some l -> Some (List.rev l)) in
	let args, varargs = (match args with
		| [{ v_name = "__arguments__" },_] -> [], true
		| _ -> args, false
	) in
	let rec loop_try e =
		match e.eexpr with
		| TFunction _ -> ()
		| TTry _ -> raise Exit
		| _ -> Type.iter loop_try e
	in
	ctx.try_scope_reg <- (try List.iter loop_try el; None with Exit -> Some (alloc_reg ctx KDynamic));
	(fun () ->
		let hasblock = ctx.block_vars <> [] || ctx.trys <> [] in
		List.iter (fun (_,v,_) ->
			try
				let p = PMap.find v ctx.used_vars in
				error ("Accessing to this member variable is prevented by local '" ^ v ^ "' used in closure") p
			with
				Not_found -> ()
		) ctx.block_vars;
		let code = DynArray.to_list ctx.code in
		let extra = (
			if hasblock then begin
				let scope = (match ctx.try_scope_reg with
					| None -> [HScope]
					| Some r -> [HDup; HSetReg r.rid; HScope]
				) in
				HThis :: HScope :: HNewBlock :: scope
			end else if this_reg then
				[HThis; HScope]
			else
				[]
		) in
		(* add dummy registers initialization *)
		let extra = extra @ List.concat (List.map (fun r ->
			if not r.rcond then
				[]
			else
			let s = [HSetReg r.rid] in
			match r.rtype with
			| KInt -> HSmallInt 0 :: s
			| KUInt -> HSmallInt 0 :: HToUInt :: s
			| KFloat -> HNaN :: s
			| KBool -> HFalse :: s
			| KType t -> HNull :: HAsType t :: s
			| KDynamic -> HNull :: HAsAny :: s
			| KNone -> HNull :: HAsType (HMPath ([],"Class")) :: s
		) (DynArray.to_list ctx.infos.iregs)) in
		let delta = List.length extra in
		let f = {
			hlf_stack_size = (if ctx.infos.imax = 0 && (hasblock || this_reg) then 1 else ctx.infos.imax);
			hlf_nregs = DynArray.length ctx.infos.iregs + 1;
			hlf_init_scope = 1;
			hlf_max_scope = ctx.infos.imaxscopes + 1 + (if hasblock then 2 else if this_reg then 1 else 0);
			hlf_code = Array.of_list (extra @ code);
			hlf_trys = Array.of_list (List.map (fun t ->
				{
					hltc_start = t.tr_pos + delta;
					hltc_end = t.tr_end + delta;
					hltc_handle = t.tr_catch_pos + delta;
					hltc_type = type_opt ctx t.tr_type;
					hltc_name = None;
				}
			) (List.rev ctx.trys));
			hlf_locals = Array.of_list (List.map (fun (id,name,t) -> ident name, t, id, false) ctx.block_vars);
		} in
		let mt = { (end_fun ctx args dparams tret) with
			hlmt_var_args = varargs;
			hlmt_new_block = hasblock;
			hlmt_function = Some f;
		} in
		ctx.locals <- old_locals;
		ctx.code <- old_code;
		ctx.infos <- old_infos;
		ctx.trys <- old_trys;
		ctx.block_vars <- old_bvars;
		ctx.in_static <- old_static;
		ctx.last_line <- last_line;
		ctx.try_scope_reg <- old_treg;
		ctx.used_vars <- old_uvars;
		mt
	)

let empty_method ctx p =
	let f = begin_fun ctx [] ctx.com.basic.tvoid [] true p in
	write ctx HRetVoid;
	f()

let begin_loop ctx =
	let old_loop = ctx.infos.iloop in
	let old_breaks = ctx.breaks in
	let old_conts = ctx.continues in
	ctx.infos.iloop <- ctx.infos.istack;
	ctx.breaks <- [];
	ctx.continues <- [];
	(fun cont_pos ->
		if ctx.infos.istack <> ctx.infos.iloop then assert false;
		List.iter (fun j -> j()) ctx.breaks;
		List.iter (fun j -> j cont_pos) ctx.continues;
		ctx.infos.iloop <- old_loop;
		ctx.breaks <- old_breaks;
		ctx.continues <- old_conts;
	)

let no_value ctx retval =
	(* does not push a null but still increment the stack like if
	   a real value was pushed *)
	if retval then ctx.infos.istack <- ctx.infos.istack + 1

let pop_value ctx retval =
	(* if we have multiple branches, make sure to forget about previous
	   branch value *)
	if retval then ctx.infos.istack <- ctx.infos.istack - 1

let gen_expr_ref = ref (fun _ _ _ -> assert false)
let gen_expr ctx e retval = (!gen_expr_ref) ctx e retval

let use_var ctx f p =
	if not (PMap.mem f ctx.used_vars) then ctx.used_vars <- PMap.add f p ctx.used_vars

let gen_access ctx e (forset : 'a) : 'a access =
	match e.eexpr with
	| TLocal v ->
		gen_local_access ctx v e.epos forset
	| TField (e1,f) | TClosure (e1,f) ->
		let id, k, closure = property ctx f e1.etype in
		if closure && not ctx.for_call then error "In Flash9, this method cannot be accessed this way : please define a local function" e1.epos;
		(match e1.eexpr with
		| TConst TThis when not ctx.in_static ->
			use_var ctx f e.epos;
			write ctx (HFindProp id)
		| _ -> gen_expr ctx true e1);
		(match k with
		| Some t -> VCast (id,t)
		| None ->
		match follow e1.etype, follow e.etype with
		| _ , TFun _ when not ctx.for_call -> VCast(id,classify ctx e.etype)
		| TEnum _, _ -> VId id
		| TInst (_,tl), et ->
			(* if the return type is one of the type-parameters, then we need to cast it *)
			if List.exists (fun t -> follow t == et) tl then
				VCast (id, classify ctx et)
			else if Codegen.is_volatile e.etype then
				VVolatile (id,None)
			else
				VId id
		| TAnon a, _ when (match !(a.a_status) with Statics _ | EnumStatics _ -> true | _ -> false) ->
			if Codegen.is_volatile e.etype then
				VVolatile (id,None)
			else
				VId id
		| _ ->
			if Codegen.is_volatile e.etype then
				VVolatile (id,Some (classify ctx e.etype))
			else
				VCast (id,classify ctx e.etype)
		)
	| TArray ({ eexpr = TLocal { v_name = "__global__" } },{ eexpr = TConst (TString s) }) ->
		let path = parse_path s in
		let id = type_path ctx path in
		if is_set forset then write ctx HGetGlobalScope;
		VGlobal id
	| TArray (e,eindex) ->
		gen_expr ctx true e;
		gen_expr ctx true eindex;
		VArray
	| TTypeExpr t ->
		let id = type_path ctx (t_path t) in
		if is_set forset then write ctx HGetGlobalScope;
		VGlobal id
	| _ ->
		invalid_expr e.epos

let gen_expr_twice ctx e =
	match e.eexpr with
	| TLocal v ->
		(match get_local_register ctx v with
		| Some r ->
			write ctx (HReg r.rid);
			write ctx (HReg r.rid);
		| None ->
			gen_expr ctx true e;
			write ctx HDup)
	| TConst _ ->
		gen_expr ctx true e;
		gen_expr ctx true e;
	| _ ->
		gen_expr ctx true e;
		write ctx HDup

let gen_access_rw ctx e : (read access * write access) =
	match e.eexpr with
	| TArray ({ eexpr = TLocal _ }, { eexpr = TConst _ })
	| TArray ({ eexpr = TLocal _ }, { eexpr = TLocal _ })
	| TField ({ eexpr = TLocal _ },_)
	| TField ({ eexpr = TConst _ },_)
	->
		let w = gen_access ctx e Write in
		let r = gen_access ctx e Read in
		r, w
	| TArray (e,eindex) ->
		let r = (match e.eexpr with TLocal v -> get_local_register ctx v | _ -> None) in
		(match r with
		| None ->
			let r = alloc_reg ctx (classify ctx e.etype) in
			gen_expr ctx true e;
			set_reg ctx r;
			write ctx (HReg r.rid);
			gen_expr_twice ctx eindex;
			write ctx (HReg r.rid);
			write ctx HSwap;
			free_reg ctx r;
		| Some r ->
			write ctx (HReg r.rid);
			gen_expr_twice ctx eindex;
			write ctx (HReg r.rid);
			write ctx HSwap;
		);
		VArray, VArray
	| TField _ ->
		let w = gen_access ctx e Write in
		write ctx HDup;
		Obj.magic w, w
	| _ ->
		let w = gen_access ctx e Write in
		let r = gen_access ctx e Read in
		r, w

let rec gen_type ctx t =
	match t with
	| HMParams (t,tl) ->
		write ctx (HGetLex t);
		List.iter (gen_type ctx) tl;
		write ctx (HApplyType (List.length tl));
	| _ ->
		write ctx (HGetLex t)

let rec gen_expr_content ctx retval e =
	match e.eexpr with
	| TConst c ->
		gen_constant ctx c e.etype e.epos
	| TThrow e ->
		ctx.infos.icond <- true;
		getvar ctx (VGlobal (type_path ctx (["flash"],"Boot")));
		let id = type_path ctx (["flash";"errors"],"Error") in
		write ctx (HFindPropStrict id);
		write ctx (HConstructProperty (id,0));
		setvar ctx (VId (ident "lastError")) None;
		gen_expr ctx true e;
		write ctx HThrow;
		no_value ctx retval;
	| TParenthesis e ->
		gen_expr ctx retval e
	| TEnumField (e,s) ->
		let id = type_path ctx e.e_path in
		write ctx (HGetLex id);
		write ctx (HGetProp (ident s));
	| TObjectDecl fl ->
		List.iter (fun (name,e) ->
			write ctx (HString name);
			gen_expr ctx true e
		) fl;
		write ctx (HObject (List.length fl))
	| TArrayDecl el ->
		List.iter (gen_expr ctx true) el;
		write ctx (HArray (List.length el))
	| TBlock el ->
		let rec loop = function
			| [] ->
				if retval then write ctx HNull
			| [e] ->
				gen_expr ctx retval e
			| e :: l ->
				gen_expr ctx false e;
				loop l
		in
		let b = open_block ctx retval in
		loop el;
		b();
	| TVars vl ->
		List.iter (fun (v,ei) ->
			define_local ctx v e.epos;
			(match ei with
			| None -> ()
			| Some e ->
				let acc = gen_local_access ctx v e.epos Write in
				gen_expr ctx true e;
				setvar ctx acc None)
		) vl
	| TReturn None ->
		write ctx HRetVoid;
		ctx.infos.icond <- true;
		no_value ctx retval
	| TReturn (Some e) ->
		gen_expr ctx true e;
		write ctx HRet;
		ctx.infos.icond <- true;
		no_value ctx retval
	| TField _
	| TClosure _
	| TLocal _
	| TTypeExpr _ ->
		getvar ctx (gen_access ctx e Read)
	| TArray _ ->
		getvar ctx (gen_access ctx e Read);
		coerce ctx (classify ctx e.etype)
	| TBinop (op,e1,e2) ->
		gen_binop ctx retval op e1 e2 e.etype
	| TCall (f,el) ->
		gen_call ctx retval f el e.etype
	| TNew ({ cl_path = [],"Array" },_,[]) ->
		(* it seems that [] is 4 time faster than new Array() *)
		write ctx (HArray 0)
	| TNew (c,tl,pl) ->
		let id = type_id ctx (TInst (c,tl)) in
		(match id with
		| HMParams _ ->
			gen_type ctx id;
			List.iter (gen_expr ctx true) pl;
			write ctx (HConstruct (List.length pl))
		| _ ->
			write ctx (HFindPropStrict id);
			List.iter (gen_expr ctx true) pl;
			write ctx (HConstructProperty (id,List.length pl))
		);
	| TFunction f ->
		write ctx (HFunction (generate_function ctx f true))
	| TIf (e0,e1,e2) ->
		let j = jump_expr ctx e0 false in
		let branch = begin_branch ctx in
		gen_expr ctx retval e1;
		let t = classify ctx e.etype in
		if retval && classify ctx e1.etype <> t then coerce ctx t;
		(match e2 with
		| None -> j()
		| Some e ->
			(* two expresssions, but one per branch *)
			pop_value ctx retval;
			let jend = jump ctx J3Always in
			j();
			gen_expr ctx retval e;
			if retval && classify ctx e.etype <> t then coerce ctx t;
			jend());
		branch();
	| TWhile (econd,e,flag) ->
		let jstart = jump ctx J3Always in
		let end_loop = begin_loop ctx in
		let branch = begin_branch ctx in
		let loop = jump_back ctx in
		if flag = DoWhile then jstart();
		gen_expr ctx false e;
		if flag = NormalWhile then jstart();
		let continue_pos = ctx.infos.ipos in
		let _ = jump_expr_gen ctx econd true (fun j -> loop j; (fun() -> ())) in
		branch();
		end_loop continue_pos;
		if retval then write ctx HNull
	| TUnop (op,flag,e) ->
		gen_unop ctx retval op flag e
	| TTry (e2,cases) ->
		if ctx.infos.istack <> 0 then error "Cannot compile try/catch as a right-side expression in Flash9" e.epos;
		let branch = begin_branch ctx in
		let p = ctx.infos.ipos in
		gen_expr ctx retval e2;
		let pend = ctx.infos.ipos in
		let jend = jump ctx J3Always in
		let rec loop ncases = function
			| [] -> []
			| (v,e) :: l ->
				let b = open_block ctx retval in
				let t = v.v_type in
				ctx.trys <- {
					tr_pos = p;
					tr_end = pend;
					tr_catch_pos = ctx.infos.ipos;
					tr_type = t;
				} :: ctx.trys;
				ctx.infos.istack <- ctx.infos.istack + 1;
				if ctx.infos.imax < ctx.infos.istack then ctx.infos.imax <- ctx.infos.istack;
				write ctx HThis;
				write ctx HScope;
				write ctx (HReg (match ctx.try_scope_reg with None -> assert false | Some r -> r.rid));
				write ctx HScope;
				(* store the exception into local var, using a tmp register if needed *)
				define_local ctx v e.epos;
				let r = (match snd (try PMap.find v.v_id ctx.locals with Not_found -> assert false) with
					| LReg _ -> None
					| _ ->
						let r = alloc_reg ctx (classify ctx t) in
						set_reg ctx r;
						Some r
				) in
				let acc = gen_local_access ctx v e.epos Write in
				(match r with None -> () | Some r -> write ctx (HReg r.rid));
				setvar ctx acc None;
				(* ----- *)
				let rec call_loop e =
					match e.eexpr with
					| TCall _ | TNew _ -> raise Exit
					| TFunction _ -> ()
					| _ -> Type.iter call_loop e
				in
				let has_call = (try call_loop e; false with Exit -> true) in
				if has_call then begin
					getvar ctx (gen_local_access ctx v e.epos Read);
					write ctx (HAsType (type_path ctx (["flash";"errors"],"Error")));
					let j = jump ctx J3False in
					getvar ctx (VGlobal (type_path ctx (["flash"],"Boot")));
					getvar ctx (gen_local_access ctx v e.epos Read);
					setvar ctx (VId (ident "lastError")) None;
					j();
				end;
				gen_expr ctx retval e;
				b();
				if retval then ctx.infos.istack <- ctx.infos.istack - 1;
				match l with
				| [] -> []
				| _ ->
					let j = jump ctx J3Always in
					j :: loop (ncases + 1) l
		in
		let loops = loop (List.length ctx.trys) cases in
		List.iter (fun j -> j()) loops;
		branch();
		jend()
	| TFor (v,it,e) ->
		gen_expr ctx true it;
		let r = alloc_reg ctx KDynamic in
		set_reg ctx r;
		let branch = begin_branch ctx in
		let b = open_block ctx retval in
		define_local ctx v e.epos;
		let end_loop = begin_loop ctx in
		let continue_pos = ctx.infos.ipos in
		let start = jump_back ctx in
		write ctx (HReg r.rid);
		write ctx (HCallProperty (ident "hasNext",0));
		let jend = jump ctx J3False in
		let acc = gen_local_access ctx v e.epos Write in
		write ctx (HReg r.rid);
		write ctx (HCallProperty (ident "next",0));
		setvar ctx acc None;
		gen_expr ctx false e;
		start J3Always;
		end_loop continue_pos;
		jend();
		if retval then getvar ctx (gen_local_access ctx v e.epos Read);
		b();
		branch();
		free_reg ctx r;
	| TBreak ->
		pop ctx (ctx.infos.istack - ctx.infos.iloop);
		ctx.breaks <- jump ctx J3Always :: ctx.breaks;
		no_value ctx retval
	| TContinue ->
		pop ctx (ctx.infos.istack - ctx.infos.iloop);
		let op = DynArray.length ctx.code in
		let p = ctx.infos.ipos in
		write ctx (HJump (J3Always,0));
		ctx.continues <- (fun target -> DynArray.set ctx.code op (HJump (J3Always,target - p))) :: ctx.continues;
		no_value ctx retval
	| TSwitch (e0,el,eo) ->
		let t = classify ctx e.etype in
		(try
			let t0 = classify ctx e0.etype in
			(* generate optimized int switch *)
			if t0 <> KInt && t0 <> KUInt then raise Exit;
			let rec get_int e =
				match e.eexpr with
				| TConst (TInt n) -> if n < 0l || n > 512l then raise Exit; Int32.to_int n
				| TParenthesis e | TBlock [e] -> get_int e
				| _ -> raise Not_found
			in
			List.iter (fun (vl,_) -> List.iter (fun v ->
				try ignore (get_int v) with _ -> raise Exit
			) vl) el;
			gen_expr ctx true e0;
			if t0 <> KInt then write ctx HToInt;
			let switch, case = begin_switch ctx in
			(match eo with
			| None ->
				if retval then begin
					write ctx HNull;
					coerce ctx t;
				end;
			| Some e ->
				gen_expr ctx retval e;
				if retval && classify ctx e.etype <> t then coerce ctx t);
			let jends = List.map (fun (vl,e) ->
				let j = jump ctx J3Always in
				List.iter (fun v -> case (get_int v)) vl;
				pop_value ctx retval;
				gen_expr ctx retval e;
				if retval && classify ctx e.etype <> t then coerce ctx t;
				j
			) el in
			List.iter (fun j -> j()) jends;
			switch();
		with Exit ->
		let r = alloc_reg ctx (classify ctx e0.etype) in
		gen_expr ctx true e0;
		set_reg ctx r;
		let branch = begin_branch ctx in
		let prev = ref (fun () -> ()) in
		let jend = List.map (fun (vl,e) ->
			(!prev)();
			let rec loop = function
				| [] ->
					assert false
				| [v] ->
					write ctx (HReg r.rid);
					gen_expr ctx true v;
					prev := jump ctx J3Neq;
				| v :: l ->
					write ctx (HReg r.rid);
					gen_expr ctx true v;
					let j = jump ctx J3Eq in
					loop l;
					j()
			in
			loop vl;
			gen_expr ctx retval e;
			pop_value ctx retval;
			if retval && classify ctx e.etype <> t then coerce ctx t;
			jump ctx J3Always
		) el in
		(!prev)();
		free_reg ctx r;
		(match eo with
		| None ->
			if retval then begin
				write ctx HNull;
				coerce ctx t;
			end;
		| Some e ->
			gen_expr ctx retval e;
			if retval && classify ctx e.etype <> t then coerce ctx t;
		);
		List.iter (fun j -> j()) jend;
		branch());
	| TMatch (e0,_,cases,def) ->
		let t = classify ctx e.etype in
		let rparams = alloc_reg ctx (KType (type_path ctx ([],"Array"))) in
		let has_params = List.exists (fun (_,p,_) -> p <> None) cases in
		gen_expr ctx true e0;
		if has_params then begin
			write ctx HDup;
			write ctx (HGetProp (ident "params"));
			set_reg ctx rparams;
		end;
		write ctx (HGetProp (ident "index"));
		write ctx HToInt;
		let switch,case = begin_switch ctx in
		(match def with
		| None ->
			if retval then begin
				write ctx HNull;
				coerce ctx t;
			end;
		| Some e ->
			gen_expr ctx retval e;
			if retval && classify ctx e.etype <> t then coerce ctx t);
		let jends = List.map (fun (cl,params,e) ->
			let j = jump ctx J3Always in
			List.iter case cl;
			pop_value ctx retval;
			let b = open_block ctx retval in
			(match params with
			| None -> ()
			| Some l ->
				let p = ref (-1) in
				List.iter (fun v ->
					incr p;
					match v with
					| None -> ()
					| Some v ->
						define_local ctx v e.epos;
						let acc = gen_local_access ctx v e.epos Write in
						write ctx (HReg rparams.rid);
						write ctx (HSmallInt !p);
						getvar ctx VArray;
						setvar ctx acc None
				) l
			);
			gen_expr ctx retval e;
			b();
			if retval && classify ctx e.etype <> t then coerce ctx t;
			j
		) cases in
		switch();
		List.iter (fun j -> j()) jends;
		free_reg ctx rparams
	| TCast (e1,t) ->
		gen_expr ctx retval e1;
		if retval then begin
			match t with
			| None ->
				(* no error if cast failure *)
				let t1 = classify ctx e1.etype in
				let t = classify ctx e.etype in
				if t1 <> t then coerce ctx t;
			| Some t ->
				(* manual cast *)
				let tid = (match gen_access ctx (mk (TTypeExpr t) t_dynamic e.epos) Read with
					| VGlobal id -> id
					| _ -> assert false
				) in
				match classify ctx e.etype with
				| KType n when (match n with HMPath ([],"String") -> false | _ -> true) ->
					(* for normal classes, we can use native cast *)
					write ctx (HCast tid)
				| _ ->
					(* we need to check with "is" first *)
					write ctx HDup;
					write ctx (HIsType tid);
					let j = jump ctx J3True in
					write ctx (HString "Class cast error");
					write ctx HThrow;
					j();
					write ctx (HCast tid)
		end

and gen_call ctx retval e el r =
	match e.eexpr , el with
	| TLocal { v_name = "__is__" }, [e;t] ->
		gen_expr ctx true e;
		gen_expr ctx true t;
		write ctx (HOp A3OIs)
	| TLocal { v_name = "__as__" }, [e;t] ->
		gen_expr ctx true e;
		gen_expr ctx true t;
		write ctx (HOp A3OAs)
	| TLocal { v_name = "__int__" }, [e] ->
		gen_expr ctx true e;
		write ctx HToInt
	| TLocal { v_name = "__float__" }, [e] ->
		gen_expr ctx true e;
		write ctx HToNumber
	| TLocal { v_name = "__foreach__" }, [obj;counter] ->
		gen_expr ctx true obj;
		gen_expr ctx true counter;
		write ctx HForEach
	| TLocal { v_name = "__forin__" }, [obj;counter] ->
		gen_expr ctx true obj;
		gen_expr ctx true counter;
		write ctx HForIn
	| TLocal { v_name = "__has_next__" }, [obj;counter] ->
		let oreg = match gen_access ctx obj Read with VReg r -> r | _ -> error "Must be a local variable" obj.epos in
		let creg = match gen_access ctx counter Read with VReg r -> r | _ -> error "Must be a local variable" obj.epos in
		write ctx (HNext (oreg.rid,creg.rid))
	| TLocal { v_name = "__hkeys__" }, [e2]
	| TLocal { v_name = "__foreach__" }, [e2]
	| TLocal { v_name = "__keys__" }, [e2] ->
		let racc = alloc_reg ctx (KType (type_path ctx ([],"Array"))) in
		let rcounter = alloc_reg ctx KInt in
		let rtmp = alloc_reg ctx KDynamic in
		write ctx (HSmallInt 0);
		set_reg ctx rcounter;
		write ctx (HArray 0);
		set_reg ctx racc;
		gen_expr ctx true e2;
		set_reg ctx rtmp;
		let start = jump ctx J3Always in
		let loop = jump_back ctx in
		write ctx (HReg racc.rid);
		write ctx (HReg rtmp.rid);
		write ctx (HReg rcounter.rid);
		(match e.eexpr with
		| TLocal { v_name = "__foreach__" } ->
			write ctx HForEach
		| TLocal { v_name = "__hkeys__" } ->
			write ctx HForIn;
			write ctx (HSmallInt 1);
			write ctx (HCallProperty (as3 "substr",1));
		| _ ->
			write ctx HForIn);
		write ctx (HCallPropVoid (as3 "push",1));
		start();
		write ctx (HNext (rtmp.rid,rcounter.rid));
		loop J3True;
		write ctx (HReg racc.rid);
		free_reg ctx rtmp;
		free_reg ctx rcounter;
		free_reg ctx racc;
	| TLocal { v_name = "__new__" }, e :: el ->
		gen_expr ctx true e;
		List.iter (gen_expr ctx true) el;
		write ctx (HConstruct (List.length el))
	| TLocal { v_name = "__delete__" }, [o;f] ->
		gen_expr ctx true o;
		gen_expr ctx true f;
		write ctx (HDeleteProp dynamic_prop);
	| TLocal { v_name = "__unprotect__" }, [e] ->
		write ctx (HGetLex (type_path ctx (["flash"],"Boot")));
		gen_expr ctx true e;
		write ctx (HCallProperty (ident "__unprotect__",1));
	| TLocal { v_name = "__typeof__" }, [e] ->
		gen_expr ctx true e;
		write ctx HTypeof
	| TLocal { v_name = "__in__" }, [e; f] ->
		gen_expr ctx true e;
		gen_expr ctx true f;
		write ctx (HOp A3OIn)
	| TLocal { v_name = "__resources__" }, [] ->
		let count = ref 0 in
		Hashtbl.iter (fun name data ->
			incr count;
			write ctx (HString "name");
			write ctx (HString name);
			write ctx (HObject 1);
		) ctx.com.resources;
		write ctx (HArray !count)
	| TLocal { v_name = "__vmem_set__" }, [{ eexpr = TConst (TInt code) };e1;e2] ->
		gen_expr ctx true e2;
		gen_expr ctx true e1;
		write ctx (HOp (match code with
			| 0l -> A3OMemSet8
			| 1l -> A3OMemSet16
			| 2l -> A3OMemSet32
			| 3l -> A3OMemSetFloat
			| 4l -> A3OMemSetDouble
			| _ -> assert false
		))
	| TLocal { v_name = "__vmem_get__" }, [{ eexpr = TConst (TInt code) };e] ->
		gen_expr ctx true e;
		write ctx (HOp (match code with
			| 0l -> A3OMemGet8
			| 1l -> A3OMemGet16
			| 2l -> A3OMemGet32
			| 3l -> A3OMemGetFloat
			| 4l -> A3OMemGetDouble
			| _ -> assert false
		))
	| TLocal { v_name = "__vmem_sign__" }, [{ eexpr = TConst (TInt code) };e] ->
		gen_expr ctx true e;
		write ctx (HOp (match code with
			| 0l -> A3OSign1
			| 1l -> A3OSign8
			| 2l -> A3OSign16
			| _ -> assert false
		))
	| TLocal { v_name = "__vector__" }, [ep] ->
		gen_type ctx (type_id ctx r);
		write ctx HGetGlobalScope;
		gen_expr ctx true ep;
		write ctx (HCallStack 1)
	| TArray ({ eexpr = TLocal { v_name = "__global__" } },{ eexpr = TConst (TString s) }), _ ->
		(match gen_access ctx e Read with
		| VGlobal id ->
			write ctx (HFindPropStrict id);
			List.iter (gen_expr ctx true) el;
			write ctx (HCallProperty (id,List.length el));
		| _ -> assert false)
	| TConst TSuper , _ ->
		write ctx HThis;
		List.iter (gen_expr ctx true) el;
		write ctx (HConstructSuper (List.length el));
	| TField ({ eexpr = TConst TSuper },f) , _ ->
		use_var ctx f e.epos;
		let id = ident f in
		write ctx (HFindPropStrict id);
		List.iter (gen_expr ctx true) el;
		write ctx (HCallSuper (id,List.length el));
		coerce ctx (classify ctx r);
	| TField ({ eexpr = TConst TThis },f) , _ when not ctx.in_static ->
		use_var ctx f e.epos;
		let id = ident f in
		write ctx (HFindProp id);
		List.iter (gen_expr ctx true) el;
		if retval then begin
			write ctx (HCallProperty (id,List.length el));
			coerce ctx (classify ctx r);
		end else
			write ctx (HCallPropVoid (id,List.length el))
	| TField (e1,f) , _ ->
		let old = ctx.for_call in
		ctx.for_call <- true;
		gen_expr ctx true e1;
		ctx.for_call <- old;
		List.iter (gen_expr ctx true) el;
		let id , _, _ = property ctx f e1.etype in
		if retval then begin
			write ctx (HCallProperty (id,List.length el));
			coerce ctx (classify ctx r);
		end else
			write ctx (HCallPropVoid (id,List.length el))
	| TEnumField (e,f) , _ ->
		let id = type_path ctx e.e_path in
		write ctx (HGetLex id);
		List.iter (gen_expr ctx true) el;
		write ctx (HCallProperty (ident f,List.length el));
	| _ ->
		gen_expr ctx true e;
		write ctx HGetGlobalScope;
		List.iter (gen_expr ctx true) el;
		write ctx (HCallStack (List.length el));
		coerce ctx (classify ctx r)

and gen_unop ctx retval op flag e =
	let k = classify ctx e.etype in
	match op with
	| Not ->
		gen_expr ctx true e;
		write ctx (HOp A3ONot);
	| Neg ->
		gen_expr ctx true e;
		write ctx (HOp (if k = KInt then A3OINeg else A3ONeg));
	| NegBits ->
		gen_expr ctx true e;
		write ctx (HOp A3OBitNot);
	| Increment
	| Decrement ->
		let incr = (op = Increment) in
		let r = (match e.eexpr with TLocal v -> get_local_register ctx v | _ -> None) in
		match r with
		| Some r when r.rtype = KInt ->
			if not r.rinit then r.rcond <- true;
			if retval && flag = Postfix then getvar ctx (VReg r);
			write ctx (if incr then HIncrIReg r.rid else HDecrIReg r.rid);
			if retval && flag = Prefix then getvar ctx (VReg r);
		| _ ->
		let acc_read, acc_write = gen_access_rw ctx e in
		let op = (match k, incr with
			| KInt, true -> A3OIIncr
			| KInt, false -> A3OIDecr
			| _ , true -> A3OIncr
			| _ , false -> A3ODecr
		) in
		getvar ctx acc_read;
		match flag with
		| Postfix when retval ->
			let r = alloc_reg ctx k in
			write ctx HDup;
			set_reg ctx r;
			write ctx (HOp op);
			setvar ctx acc_write None;
			write ctx (HReg r.rid);
			free_reg ctx r
		| Postfix | Prefix ->
			write ctx (HOp op);
			setvar ctx acc_write (if retval then Some k else None)

and check_binop ctx e1 e2 =
	let invalid = (match classify ctx e1.etype, classify ctx e2.etype with
	| KInt, KUInt | KUInt, KInt -> (match e1.eexpr, e2.eexpr with TConst (TInt i) , _ | _ , TConst (TInt i) -> i < 0l | _ -> true)
	| _ -> false) in
	if invalid then error "Comparison of Int and UInt might lead to unexpected results" (punion e1.epos e2.epos);

and gen_binop ctx retval op e1 e2 t =
	let write_op op =
		let iop = (match op with
			| OpAdd -> Some A3OIAdd
			| OpSub -> Some A3OISub
			| OpMult -> Some A3OIMul
			| _ -> None
		) in
		let op = (match op with
			| OpAdd -> A3OAdd
			| OpSub -> A3OSub
			| OpMult -> A3OMul
			| OpDiv -> A3ODiv
			| OpAnd -> A3OAnd
			| OpOr -> A3OOr
			| OpXor -> A3OXor
			| OpShl -> A3OShl
			| OpShr -> A3OShr
			| OpUShr -> A3OUShr
			| OpMod -> A3OMod
			| _ -> assert false
		) in
		match iop with
		| Some iop ->
			let k1 = classify ctx e1.etype in
			let k2 = classify ctx e2.etype in
			(match k1, k2 with
			| KInt, KInt | KUInt, KUInt | KInt, KUInt | KUInt, KInt -> write ctx (HOp iop)
			| _ ->
				write ctx (HOp op);
				(* add is a generic operation, so let's make sure we don't loose our type in the process *)
				if op = A3OAdd then coerce ctx (classify ctx t))
		| _ ->
			write ctx (HOp op);
			if op = A3OMod && classify ctx e1.etype = KInt && classify ctx e2.etype = KInt then coerce ctx (classify ctx t);
	in
	let gen_op o =
		check_binop ctx e1 e2;
		gen_expr ctx true e1;
		gen_expr ctx true e2;
		write ctx (HOp o)
	in
	match op with
	| OpAssign ->
		let acc = gen_access ctx e1 Write in
		gen_expr ctx true e2;
		setvar ctx acc (if retval then Some (classify ctx e1.etype) else None)
	| OpBoolAnd ->
		write ctx HFalse;
		let j = jump_expr ctx e1 false in
		let b = begin_branch ctx in
		write ctx HPop;
		gen_expr ctx true e2;
		coerce ctx KBool;
		j();
		b();
	| OpBoolOr ->
		write ctx HTrue;
		let j = jump_expr ctx e1 true in
		let b = begin_branch ctx in
		write ctx HPop;
		gen_expr ctx true e2;
		coerce ctx KBool;
		j();
		b();
	| OpAssignOp op ->
		let racc, wacc = gen_access_rw ctx e1 in
		getvar ctx racc;
		gen_expr ctx true e2;
		write_op op;
		setvar ctx wacc (if retval then Some (classify ctx e1.etype) else None)
	| OpAdd | OpMult | OpDiv | OpSub | OpAnd | OpOr | OpXor | OpShl | OpShr | OpUShr | OpMod ->
		gen_expr ctx true e1;
		gen_expr ctx true e2;
		write_op op
	| OpEq ->
		gen_op A3OEq
	| OpNotEq ->
		gen_op A3OEq;
		write ctx (HOp A3ONot)
	| OpGt ->
		gen_op A3OGt
	| OpGte ->
		gen_op A3OGte
	| OpLt ->
		gen_op A3OLt
	| OpLte ->
		gen_op A3OLte
	| OpInterval ->
		assert false

and gen_expr ctx retval e =
	let old = ctx.infos.istack in
	debug_infos ctx e.epos;
	gen_expr_content ctx retval e;
	if old <> ctx.infos.istack then begin
		if old + 1 <> ctx.infos.istack then stack_error e.epos;
		if not retval then write ctx HPop;
	end else if retval then stack_error e.epos

and generate_function ctx fdata stat =
	let f = begin_fun ctx fdata.tf_args fdata.tf_type [fdata.tf_expr] stat fdata.tf_expr.epos in
	gen_expr ctx false fdata.tf_expr;
	(match follow fdata.tf_type with
	| TEnum ({ e_path = [],"Void" },[]) ->
		debug_infos ctx ~is_min:false fdata.tf_expr.epos;
		write ctx HRetVoid
	| _ ->
		(* check that we have a return that can be accepted by Flash9 VM *)
		let rec loop e =
			match e.eexpr with
			| TBlock [] -> false
			| TBlock l -> loop (List.hd (List.rev l))
			| TReturn None -> true
			| TReturn (Some e) ->
				let rec inner_loop e =
					match e.eexpr with
					| TSwitch _ | TMatch _ | TFor _ | TWhile _ | TTry _ -> false
					| TIf _ -> loop e
					| TParenthesis e -> inner_loop e
					| _ -> true
				in
				inner_loop e
			| TIf (_,e1,Some e2) -> loop e1 && loop e2
			| TSwitch (_,_,Some e) -> loop e
			| TParenthesis e -> loop e
			| _ -> false
		in
		if not (loop fdata.tf_expr) then write ctx HRetVoid;
	);
	f()

and jump_expr_gen ctx e jif jfun =
	match e.eexpr with
	| TParenthesis e -> jump_expr_gen ctx e jif jfun
	| TBinop (op,e1,e2) ->
		let j t f =
			check_binop ctx e1 e2;
			gen_expr ctx true e1;
			gen_expr ctx true e2;
			jfun (if jif then t else f)
		in
		(match op with
		| OpEq -> j J3Eq J3Neq
		| OpNotEq -> j J3Neq J3Eq
		| OpGt -> j J3Gt J3NotGt
		| OpGte -> j J3Gte J3NotGte
		| OpLt -> j J3Lt J3NotLt
		| OpLte -> j J3Lte J3NotLte
		| _ ->
			gen_expr ctx true e;
			jfun (if jif then J3True else J3False))
	| _ ->
		gen_expr ctx true e;
		jfun (if jif then J3True else J3False)

and jump_expr ctx e jif =
	jump_expr_gen ctx e jif (jump ctx)

let generate_method ctx fdata stat =
	generate_function ctx fdata stat

let generate_construct ctx fdata c =
	(* make all args optional to allow no-param constructor *)
	let cargs = List.map (fun (v,c) ->
		let c = (match c with Some _ -> c | None ->
			Some (match classify ctx v.v_type with
			| KInt | KUInt -> TInt 0l
			| KFloat -> TFloat "0"
			| KBool -> TBool false
			| KType _ | KDynamic | KNone -> TNull)
		) in
		v,c
	) fdata.tf_args in
	let f = begin_fun ctx cargs fdata.tf_type [ethis;fdata.tf_expr] false fdata.tf_expr.epos in
	(* if skip_constructor, then returns immediatly *)
	(match c.cl_kind with
	| KGenericInstance _ -> ()
	| _ when not (Codegen.constructor_side_effects fdata.tf_expr) -> ()
	| _ ->
		let id = ident "skip_constructor" in
		getvar ctx (VGlobal (type_path ctx (["flash"],"Boot")));
		getvar ctx (VId id);
		let j = jump ctx J3False in
		write ctx HRetVoid;
		j());
	(* --- *)
	PMap.iter (fun _ f ->
		match f.cf_expr, f.cf_kind with
		| Some { eexpr = TFunction fdata }, Method MethDynamic ->
			let id = ident f.cf_name in
			write ctx (HFindProp id);
			write ctx (HGetProp id);
			let j = jump ctx J3True in
			write ctx (HFindProp id);
			write ctx (HFunction (generate_method ctx fdata false));
			write ctx (HInitProp id);
			j();
		| _ -> ()
	) c.cl_fields;
	gen_expr ctx false fdata.tf_expr;
	debug_infos ctx ~is_min:false fdata.tf_expr.epos;
	write ctx HRetVoid;
	f() , List.length fdata.tf_args

let rec is_const e =
	match e.eexpr with
	| TConst _ -> true
	| TArrayDecl el | TBlock el -> List.for_all is_const el
	| TObjectDecl fl -> List.for_all (fun (_,e) -> is_const e) fl
	| TParenthesis e -> is_const e
	| TFunction _ -> true
	| _ -> false

let generate_class_statics ctx c const =
	List.iter (fun f ->
		match f.cf_expr with
		| Some { eexpr = TFunction _ } when (match f.cf_kind with Method (MethNormal | MethInline) -> true | _ -> false) -> ()
		| Some e when is_const e = const ->
			write ctx (HGetLex (type_path ctx c.cl_path));
			gen_expr ctx true e;
			if Codegen.is_volatile f.cf_type then write ctx (HArray 1);
			write ctx (HInitProp (ident f.cf_name));
		| _ -> ()
	) c.cl_ordered_statics

let need_init ctx c =
	not ctx.swc && not c.cl_extern && List.exists (fun f -> match f.cf_expr with Some e -> not (is_const e) | _ -> false) c.cl_ordered_statics

let generate_extern_inits ctx =
	List.iter (fun t ->
		match t with
		| TClassDecl c when c.cl_extern ->
			(match c.cl_init with
			| None -> ()
			| Some e -> gen_expr ctx false e);
		| _ -> ()
	) ctx.com.types

let generate_inits ctx =
	let finit = begin_fun ctx [] ctx.com.basic.tvoid [] true null_pos in
	if not ctx.swc then generate_extern_inits ctx;
	List.iter (fun t ->
		match t with
		| TClassDecl c when need_init ctx c ->
			let id = ident "init__" in
			getvar ctx (VGlobal (type_path ctx c.cl_path));
			getvar ctx (VId id);
			let j = jump ctx J3True in
			getvar ctx (VGlobal (type_path ctx c.cl_path));
			write ctx HTrue;
			setvar ctx (VId id) None;
			let branch = begin_branch ctx in
			generate_class_statics ctx c false;
			branch();
			j()
		| _ -> ()
	) ctx.com.types;
	(match ctx.com.main with
	| None -> ()
	| Some e -> gen_expr ctx false e);
	write ctx HRetVoid;
	finit()

let generate_class_init ctx c hc =
	write ctx HGetGlobalScope;
	if c.cl_interface then
		write ctx HNull
	else begin
		let path = (match c.cl_super with None -> ([],"Object") | Some (sup,_) -> sup.cl_path) in
		write ctx (HGetLex (type_path ctx path));
		write ctx HScope;
		write ctx (HGetLex (type_path ctx path));
	end;
	write ctx (HClassDef hc);
	List.iter (fun f ->
		match f.cf_expr, f.cf_kind with
		| Some { eexpr = TFunction fdata }, Method MethDynamic ->
			write ctx HDup;
			write ctx (HFunction (generate_method ctx fdata true));
			write ctx (HInitProp (ident f.cf_name));
		| _ -> ()
	) c.cl_ordered_statics;
	if not c.cl_interface then write ctx HPopScope;
	write ctx (HInitProp (type_path ctx c.cl_path));
	if ctx.swc && c.cl_path = ctx.boot then generate_extern_inits ctx;
	(match c.cl_init with
	| None -> ()
	| Some e ->
		gen_expr ctx false e;
		if ctx.block_vars <> [] then error "You can't have a local variable referenced from a closure inside __init__ (FP 10.1.53 crash)" e.epos;
	);
	generate_class_statics ctx c true;
	if ctx.swc then begin
		generate_class_statics ctx c false;
		if ctx.block_vars <> [] then error "You can't have a local variable referenced from a closure inside a static (FP 10.1.53 crash)" c.cl_pos;
	end

let generate_enum_init ctx e hc meta =
	let path = ([],"Object") in
	let name_id = type_path ctx e.e_path in
	write ctx HGetGlobalScope;
	write ctx (HGetLex (type_path ctx path));
	write ctx HScope;
	write ctx (HGetLex (type_path ctx path));
	write ctx (HClassDef hc);
	write ctx HPopScope;
	let r = alloc_reg ctx KDynamic in
	write ctx HDup;
	write ctx (HSetReg r.rid); (* needed for setslot *)
	write ctx (HInitProp name_id);
	let nslot = ref 0 in
	PMap.iter (fun _ f ->
		incr nslot;
		match f.ef_type with
		| TFun _ -> ()
		| _ ->
			write ctx (HReg r.rid);
			write ctx (HFindPropStrict name_id);
			write ctx (HString f.ef_name);
			write ctx (HInt f.ef_index);
			write ctx HNull;
			write ctx (HConstructProperty (name_id,3));
			write ctx (HSetSlot !nslot);
	) e.e_constrs;
	write ctx (HReg r.rid);
	List.iter (fun n -> write ctx (HString n)) e.e_names;
	write ctx (HArray (List.length e.e_names));
	write ctx (HSetProp (ident "__constructs__"));
	(match meta with
	| None -> ()
	| Some e ->
		write ctx (HReg r.rid);
		gen_expr ctx true e;
		write ctx (HSetProp (ident "__meta__"));
	);
	free_reg ctx r

let extract_meta meta =
	let rec loop = function
		| [] -> []
		| (":meta",[ECall ((EConst (Ident n | Type n),_),args),_],_) :: l ->
			let mk_arg (a,p) =
				match a with
				| EConst (String s) -> (None, s)
				| EBinop (OpAssign,(EConst (Ident n | Type n),_),(EConst (String s),_)) -> (Some n, s)
				| _ -> error "Invalid meta definition" p
			in
			{ hlmeta_name = n; hlmeta_data = Array.of_list (List.map mk_arg args) } :: loop l
		| _ :: l -> loop l
	in
	match loop meta with
	| [] -> None
	| l -> Some (Array.of_list l)

let generate_field_kind ctx f c stat =
	match f.cf_expr with
	| Some { eexpr = TFunction fdata } ->
		let rec loop c name =
			match c.cl_super with
			| None -> false
			| Some (c,_) ->
				PMap.exists name c.cl_fields || loop c name
		in
		(match f.cf_kind with
		| Var _ | Method MethDynamic ->
			Some (HFVar {
				hlv_type = Some (type_path ctx ([],"Function"));
				hlv_value = HVNone;
				hlv_const = false;
			})
		| _ ->
			let rec lookup_kind = function
				| [] -> f.cf_name, MK3Normal
				| (":getter",[EConst (Ident f | Type f),_],_) :: _ -> f, MK3Getter
				| (":setter",[EConst (Ident f | Type f),_],_) :: _ -> f, MK3Setter
				| _ :: l -> lookup_kind l
			in
			let name, kind = lookup_kind f.cf_meta in
			let old = ctx.debug in
			ctx.debug <- (old || has_meta ":debug" f.cf_meta) && not (has_meta ":nodebug" f.cf_meta);
			let m = generate_method ctx fdata stat in
			ctx.debug <- old;
			Some (HFMethod {
				hlm_type = m;
				hlm_final = stat;
				hlm_override = not stat && loop c name;
				hlm_kind = kind;
			})
		);
	| _ when c.cl_interface && not stat ->
		(match follow f.cf_type, f.cf_kind with
		| TFun (args,tret), Method (MethNormal | MethInline) ->
			let dparams = ref None in
			List.iter (fun (_,o,t) ->
				match !dparams with
				| None -> if o then dparams := Some [HVNone]
				| Some l -> dparams := Some (HVNone :: l)
			) args;
			let dparams = (match !dparams with None -> None | Some l -> Some (List.rev l)) in
			Some (HFMethod {
				hlm_type = end_fun ctx (List.map (fun (a,opt,t) -> alloc_var a t, (if opt then Some TNull else None)) args) dparams tret;
				hlm_final = false;
				hlm_override = false;
				hlm_kind = MK3Normal;
			})
		| _ ->
			None)
	| _ when (match f.cf_kind with Var { v_read = AccResolve } -> true | _ -> false) ->
		None
	| _ ->
		Some (HFVar {
			hlv_type = if Codegen.is_volatile f.cf_type then Some (type_path ctx ([],"Array")) else type_opt ctx f.cf_type;
			hlv_value = HVNone;
			hlv_const = false;
		})

let generate_class ctx c =
	let name = type_path ctx c.cl_path in
	let cid , cnargs = (match c.cl_constructor with
		| None ->
			if c.cl_interface then
				{ (empty_method ctx null_pos) with hlmt_function = None }, 0
			else
				generate_construct ctx {
					tf_args = [];
					tf_type = ctx.com.basic.tvoid;
					tf_expr = {
						eexpr = TBlock [];
						etype = ctx.com.basic.tvoid;
						epos = null_pos;
					}
				} c
		| Some f ->
			match f.cf_expr with
			| Some { eexpr = TFunction fdata } -> generate_construct ctx fdata c
			| _ -> assert false
	) in
	let has_protected = ref None in
	let make_name f = 
		let rec find_meta c =
			try
				let f = PMap.find f.cf_name c.cl_fields in
				if List.mem f.cf_name c.cl_overrides then raise Not_found;
				f.cf_meta
			with Not_found ->
				match c.cl_super with
				| None -> []
				| Some (c,_) -> find_meta c
		in
		let protect() =
			let p = (match c.cl_path with [], n -> n | p, n -> String.concat "." p ^ ":" ^ n) in
			has_protected := Some p;
			HMName (f.cf_name,HNProtected p)
		in
		let rec loop_meta = function
			| [] ->
				if not f.cf_public && ctx.swf_protected then
					protect()
				else
					ident f.cf_name
			| x :: l ->
				match x with
				| ((":getter" | ":setter"),[EConst (Ident f | Type f),_],_) -> ident f
				| (":ns",[EConst (String ns),_],_) -> HMName (f.cf_name,HNNamespace ns)
				| (":protected",[],_) -> protect()
				| _ -> loop_meta l
		in
		if c.cl_interface then
			HMName (f.cf_name, HNNamespace (match c.cl_path with [],n -> n | l,n -> String.concat "." l ^ ":" ^ n))
		else
			loop_meta (find_meta c)
	in
	let fields = PMap.fold (fun f acc ->
		match generate_field_kind ctx f c false with
		| None -> acc
		| Some k ->
			{
				hlf_name = make_name f;
				hlf_slot = 0;
				hlf_kind = k;
				hlf_metas = extract_meta f.cf_meta;
			} :: acc
	) c.cl_fields [] in
	let fields = if c.cl_path <> ctx.boot then fields else begin
		{
			hlf_name = make_name {
				cf_name = "init";
				cf_public = ctx.swc && ctx.swf_protected; 
				cf_meta = [];
				cf_doc = None;
				cf_pos = c.cl_pos;
				cf_type = TFun ([],t_dynamic);
				cf_params = [];
				cf_expr = None;
				cf_kind = Method MethNormal;
			};
			hlf_slot = 0;
			hlf_kind = (HFMethod {
				hlm_type = generate_inits ctx;
				hlm_final = false;
				hlm_override = true;
				hlm_kind = MK3Normal;
			});
			hlf_metas = None;
		} :: fields
	end in
	let st_field_count = ref 0 in
	let st_meth_count = ref 0 in
	let statics = List.map (fun f ->
		let k = (match generate_field_kind ctx f c true with None -> assert false | Some k -> k) in
		let count = (match k with HFMethod _ -> st_meth_count | HFVar _ -> st_field_count | _ -> assert false) in
		incr count;
		{
			hlf_name = make_name f;
			hlf_slot = !count;
			hlf_kind = k;
			hlf_metas = extract_meta f.cf_meta;
		}
	) c.cl_ordered_statics in
	let statics = if not (need_init ctx c) then statics else
		{
			hlf_name = ident "init__";
			hlf_slot = (incr st_field_count; !st_field_count);
			hlf_kind = HFVar { hlv_type = (Some (type_id ctx ctx.com.basic.tbool)); hlv_value = HVNone; hlv_const = false; };
			hlf_metas = None;
		} :: statics
	in
	let rec is_dynamic c =
		if c.cl_dynamic <> None || c.cl_array_access <> None then true
		else match c.cl_super with
		| None -> false
		| Some (c,_) -> is_dynamic c
	in
	{
		hlc_index = 0;
		hlc_name = name;
		hlc_super = (if c.cl_interface then None else Some (type_path ctx (match c.cl_super with None -> [],"Object" | Some (c,_) -> c.cl_path)));
		hlc_sealed = not (is_dynamic c);
		hlc_final = false;
		hlc_interface = c.cl_interface;
		hlc_namespace = (match !has_protected with None -> None | Some p -> Some (HNProtected p));
		hlc_implements = Array.of_list (List.map (fun (c,_) ->
			if not c.cl_interface then error "Can't implement class in Flash9" c.cl_pos;
			let pack, name = real_path c.cl_path in
			HMMultiName (Some name,[HNPublic (Some (String.concat "." pack))])
		) c.cl_implements);
		hlc_construct = cid;
		hlc_fields = Array.of_list fields;
		hlc_static_construct = empty_method ctx c.cl_pos;
		hlc_static_fields = Array.of_list statics;
	}

let generate_enum ctx e meta =
	let name_id = type_path ctx e.e_path in
	let api = ctx.com.basic in
	let f = begin_fun ctx [alloc_var "tag" api.tstring, None;alloc_var "index" api.tint, None;alloc_var "params" (mk_mono()), None] api.tvoid [ethis] false e.e_pos in
	let tag_id = ident "tag" in
	let index_id = ident "index" in
	let params_id = ident "params" in
	write ctx (HFindProp tag_id);
	write ctx (HReg 1);
	write ctx (HInitProp tag_id);
	write ctx (HFindProp index_id);
	write ctx (HReg 2);
	write ctx (HInitProp index_id);
	write ctx (HFindProp params_id);
	write ctx (HReg 3);
	write ctx (HInitProp params_id);
	write ctx HRetVoid;
	let construct = f() in
	let f = begin_fun ctx [] api.tstring [] true e.e_pos in
	write ctx (HGetLex (type_path ctx (["flash"],"Boot")));
	write ctx HThis;
	write ctx (HCallProperty (ident "enum_to_string",1));
	write ctx HRet;
	let tostring = f() in
	let st_count = ref 0 in
	let constrs = PMap.fold (fun f acc ->
		incr st_count;
		{
			hlf_name = ident f.ef_name;
			hlf_slot = !st_count;
			hlf_kind = (match f.ef_type with
				| TFun (args,_) ->
					let fdata = begin_fun ctx (List.map (fun (a,opt,t) -> alloc_var a t, (if opt then Some TNull else None)) args) (TEnum (e,[])) [] true f.ef_pos in
					write ctx (HFindPropStrict name_id);
					write ctx (HString f.ef_name);
					write ctx (HInt f.ef_index);
					let n = ref 0 in
					List.iter (fun _ -> incr n; write ctx (HReg !n)) args;
					write ctx (HArray (!n));
					write ctx (HConstructProperty (name_id,3));
					write ctx HRet;
					let fid = fdata() in
					HFMethod {
						hlm_type = fid;
						hlm_final = true;
						hlm_override = false;
						hlm_kind = MK3Normal;
					}
				| _ ->
					HFVar { hlv_type = (Some name_id); hlv_value = HVNone; hlv_const = false; }
			);
			hlf_metas = None;
		} :: acc
	) e.e_constrs [] in
	let constrs = (match meta with
		| None -> constrs
		| Some _ ->
			incr st_count;
			{
				hlf_name = ident "__meta__";
				hlf_slot = !st_count;
				hlf_kind = HFVar { hlv_type = None; hlv_value = HVNone; hlv_const = false; };
				hlf_metas = None;
			} :: constrs
	) in
	{
		hlc_index = 0;
		hlc_name = name_id;
		hlc_super = Some (type_path ctx ([],"Object"));
		hlc_sealed = true;
		hlc_final = true;
		hlc_interface = false;
		hlc_namespace = None;
		hlc_implements = [||];
		hlc_construct = construct;
		hlc_fields = [|
			{ hlf_name = tag_id; hlf_slot = 0; hlf_kind = HFVar { hlv_type = Some (HMPath ([],"String")); hlv_value = HVNone; hlv_const = false; }; hlf_metas = None };
			{ hlf_name = index_id; hlf_slot = 0; hlf_kind = HFVar { hlv_type = Some (HMPath ([],"int")); hlv_value = HVNone; hlv_const = false; }; hlf_metas = None };
			{ hlf_name = params_id; hlf_slot = 0; hlf_kind = HFVar { hlv_type = Some (HMPath ([],"Array")); hlv_value = HVNone; hlv_const = false; }; hlf_metas = None };
			{ hlf_name = ident "__enum__"; hlf_slot = 0; hlf_kind = HFVar { hlv_type = Some (HMPath ([],"Boolean")); hlv_value = HVBool true; hlv_const = true }; hlf_metas = None };
			{
				hlf_name = ident "toString";
				hlf_slot = 0;
				hlf_kind = HFMethod {
					hlm_type = tostring;
					hlm_final = true;
					hlm_override = false;
					hlm_kind = MK3Normal;
				};
				hlf_metas = None;
			};
		|];
		hlc_static_construct = empty_method ctx e.e_pos;
		hlc_static_fields = Array.of_list ({
			hlf_name = ident "__isenum";
			hlf_slot = !st_count + 2;
			hlf_kind = HFVar { hlv_type = Some (HMPath ([],"Boolean")); hlv_value = HVBool true; hlv_const = true; };
			hlf_metas = None;
		} :: {
			hlf_name = ident "__constructs__";
			hlf_slot = !st_count + 1;
			hlf_kind = HFVar { hlv_type = None; hlv_value = HVNone; hlv_const = false; };
			hlf_metas = None;
		} :: constrs);
	}

let generate_type ctx t =
	match t with
	| TClassDecl c ->
		if c.cl_path = (["flash";"_Boot"],"RealBoot") then c.cl_path <- ctx.boot;
		if c.cl_extern && (c.cl_path <> ([],"Dynamic") || has_meta ":real" c.cl_meta) then
			None
		else
			let hlc = generate_class ctx c in
			let init = begin_fun ctx [] ctx.com.basic.tvoid [ethis] false c.cl_pos in
			generate_class_init ctx c hlc;
			write ctx HRetVoid;
			Some (init(), {
				hlf_name = type_path ctx c.cl_path;
				hlf_slot = 0;
				hlf_kind = HFClass hlc;
				hlf_metas = None;
			})
	| TEnumDecl e ->
		if e.e_extern && e.e_path <> ([],"Void") then
			None
		else
			let meta = Codegen.build_metadata ctx.com t in
			let hlc = generate_enum ctx e meta in
			let init = begin_fun ctx [] ctx.com.basic.tvoid [ethis] false e.e_pos in
			generate_enum_init ctx e hlc meta;
			write ctx HRetVoid;
			Some (init(), {
				hlf_name = type_path ctx e.e_path;
				hlf_slot = 0;
				hlf_kind = HFClass hlc;
				hlf_metas = None;
			})
	| TTypeDecl _ ->
		None

let resource_path name = 
	(["_res"],"_" ^ String.concat "_" (ExtString.String.nsplit name "."))

let generate_resource ctx name =	
	let c = mk_class null_module (resource_path name) null_pos in
	c.cl_super <- Some (mk_class null_module (["flash";"utils"],"ByteArray") null_pos,[]);
	let t = TClassDecl c in
	match generate_type ctx t with
	| Some (m,f) -> (t,m,f)
	| None -> assert false

let generate com boot_name =
	let ctx = {
		com = com;
		debug = com.Common.debug;
		boot = ([],boot_name);
		debugger = Common.defined com "fdb";
		swc = Common.defined com "swc";
		swf_protected = Common.defined com "swf_protected";
		code = DynArray.create();
		locals = PMap.empty;
		infos = default_infos();
		trys = [];
		breaks = [];
		continues = [];
		block_vars = [];
		used_vars = PMap.empty;
		in_static = false;
		last_line = -1;
		last_file = "";
		try_scope_reg = None;
		for_call = false;
	} in
	let types = if ctx.swc && com.main_class = None then
		(*
			make sure that both Boot and RealBoot are the first two classes in the SWC
			this way initializing RealBoot will also run externs __init__ blocks before
			another class static is defined
		*)
		let hd = ref [] in
		let types = List.fold_left (fun acc t ->
			match t_path t with
			| ["flash";"_Boot"],"RealBoot" -> hd := !hd @ [t]; acc
			| ["flash"], "Boot" -> hd := t :: !hd; acc
			| _ -> t :: acc
		) [] com.types in
		!hd @ List.rev types
	else
		com.types
	in
	let res = Hashtbl.fold (fun name _ acc -> generate_resource ctx name :: acc) com.resources [] in
	let classes = List.fold_left (fun acc t ->
		match generate_type ctx t with
		| None -> acc
		| Some (m,f) -> (t,m,f) :: acc
	) res types in
	List.rev classes

;;
Random.self_init();
gen_expr_ref := gen_expr
