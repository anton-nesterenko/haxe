/*
 * Copyright (c) 2005, The haXe Project Contributors
 * All rights reserved.
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *   - Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   - Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE HAXE PROJECT CONTRIBUTORS "AS IS" AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE HAXE PROJECT CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
 * DAMAGE.
 */
package haxe;

private enum TemplateExpr {
	OpVar( v : String );
	OpExpr( expr :  Void -> Dynamic );
	OpIf( expr : Void -> Dynamic,  eif : TemplateExpr, eelse : TemplateExpr );
	OpStr( str : String );
	OpBlock( l : List<TemplateExpr> );
	OpForeach( expr : Void -> Dynamic, loop : TemplateExpr );
	OpMacro( name : String, params : List<TemplateExpr> );
}

private typedef Token = {
	var s : Bool;
	var p : String;
	var l : Array<String>;
}

private typedef ExprToken = {
	var s : Bool;
	var p : String;
}

class Template {

	static var splitter = ~/(::[A-Za-z0-9_ ()&|!+=\/><*.-]+::|\$\$([A-Za-z0-9_-]+)\()/;
	static var expr_splitter = ~/(\(|\)|[!+=\/><*.&|-]+)/;
	static var expr_trim = ~/^[ ]*([^ ]+)[ ]*$/;
	static var expr_int = ~/^[0-9]+$/;
	static var expr_float = ~/^([+-]?)(?=\d|,\d)\d*(,\d*)?([Ee]([+-]?\d+))?$/;

	public static var globals = Reflect.empty();

	var expr : TemplateExpr;
	var context : Dynamic;
	var macros : Dynamic;
	var stack : List<Dynamic>;
	var buf : StringBuf;

	public function new( str : String ) {
		var tokens = parseTokens(str);
		expr = parseBlock(tokens);
		if( !tokens.isEmpty() )
			throw "Unexpected '"+tokens.first().s+"'";
	}

	public function execute( context : Dynamic, ?macros : Dynamic ) {
		this.macros = macros;
		this.context = context;
		stack = new List();
		buf = new StringBuf();
		run(expr);
		return buf.toString();
	}

	function resolve( v : String ) : Dynamic {
		if( Reflect.hasField(context,v) )
			return Reflect.field(context,v);
		for( ctx in stack )
			if( Reflect.hasField(ctx,v) )
				return Reflect.field(ctx,v);
		if( v == "__current__" )
			return context;
		return Reflect.field(globals,v);
	}

	function parseTokens( data : String ) {
		var tokens = new List<Token>();
		while( splitter.match(data) ) {
			var p = splitter.matchedPos();
			if( p.pos > 0 )
				tokens.add({ p : data.substr(0,p.pos), s : true, l : null });

			// : ?
			if( data.charCodeAt(p.pos) == 58 ) {
				tokens.add({ p : data.substr(p.pos + 2,p.len - 4), s : false, l : null });
				data = splitter.matchedRight();
				continue;
			}

			// macro parse
			var parp = p.pos + p.len;
			var npar = 1;
			while( npar > 0 ) {
				var c = data.charCodeAt(parp);
				if( c == 40 )
					npar++;
				else if( c == 41 )
					npar--;
				else if( c == null )
					throw "Unclosed macro parenthesis";
				parp++;
			}
			var params = data.substr(p.pos+p.len,parp - (p.pos+p.len) - 1).split(",");
			tokens.add({ p : splitter.matched(2), s : false, l : params });
			data = data.substr(parp,data.length - parp);
		}
		if( data.length > 0 )
			tokens.add({ p : data, s : true, l : null });
		return tokens;
	}

	function parseBlock( tokens : List<Token> ) {
		var l = new List();
		while( true ) {
			var t = tokens.first();
			if( t == null )
				break;
			if( !t.s && (t.p == "end" || t.p == "else" || t.p.substr(0,7) == "elseif ") )
				break;
			l.add(parse(tokens));
		}
		if( l.length == 1 )
			return l.first();
		return OpBlock(l);
	}

	function parse( tokens : List<Token> ) {
		var t = tokens.pop();
		var p = t.p;
		if( t.s )
			return OpStr(p);
		// macro
		if( t.l != null ) {
			var pe = new List();
			for( p in t.l )
				pe.add(parseBlock(parseTokens(p)));
			return OpMacro(p,pe);
		}
		// 'end' , 'else', 'elseif' can't be found here
		if( p.substr(0,3) == "if " ) {
			p = p.substr(3,p.length - 3);
			var e = parseExpr(p);
			var eif = parseBlock(tokens);
			var t = tokens.first();
			var eelse;
			if( t == null )
				throw "Unclosed 'if'";
			if( t.p == "end" ) {
				tokens.pop();
				eelse = null;
			} else if( t.p == "else" ) {
				tokens.pop();
				eelse = parseBlock(tokens);
				t = tokens.pop();
				if( t == null || t.p != "end" )
					throw "Unclosed 'else'";
			} else { // elseif
				t.p = t.p.substr(4,t.p.length - 4);
				eelse = parse(tokens);
			}
			return OpIf(e,eif,eelse);
		}
		if( p.substr(0,8) == "foreach " ) {
			p = p.substr(8,p.length - 8);
			var e = parseExpr(p);
			var efor = parseBlock(tokens);
			var t = tokens.pop();
			if( t == null || t.p != "end" )
				throw "Unclosed 'foreach'";
			return OpForeach(e,efor);
		}
		if( expr_splitter.match(p) )
			return OpExpr(parseExpr(p));
		return OpVar(p);
	}

	function parseExpr( data : String ) {
		var l = new List<ExprToken>();
		var expr = data;
		while( expr_splitter.match(data) ) {
			var p = expr_splitter.matchedPos();
			var k = p.pos + p.len;
			if( p.pos != 0 )
				l.add({ p : data.substr(0,p.pos), s : true });
			l.add({ p : expr_splitter.matched(0), s : false });
			data = expr_splitter.matchedRight();
		}
		if( data.length != 0 )
			l.add({ p : data, s : true });
		var e;
		try {
			e = makeExpr(l);
			if( !l.isEmpty() )
				throw l.first().p;
		} catch( s : Int ) {
			throw "Unexpected '"+s+"' in "+expr;
		}
		return function() {
			try {
				return e();
			} catch( exc : Dynamic ) {
				throw "Error : "+Std.string(exc)+" in "+expr;
			}
		}
	}

	function makeConst( v : String ) : Void -> Dynamic {
		expr_trim.match(v);
		v = expr_trim.matched(1);
		if( expr_int.match(v) ) {
			var i = Std.parseInt(v);
			return function() { return i; };
		}
		if( expr_float.match(v) ) {
			var f = Std.parseFloat(v);
			return function() { return f; };
		}
		var me = this;
		return function() { return me.resolve(v); };
	}

	function makePath( e : Void -> Dynamic, l : List<ExprToken> ) {
		var p = l.first();
		if( p == null || p.p != "." )
			return e;
		l.pop();
		var field = l.pop();
		if( field == null || !field.s )
			throw field.p;
		var f = field.p;
		expr_trim.match(f);
		f = expr_trim.matched(1);
		return makePath(function() { return Reflect.field(e(),f); },l);
	}

	function makeExpr( l ) {
		return makePath(makeExpr2(l),l);
	}

	function makeExpr2( l : List<ExprToken> ) : Void -> Dynamic {
		var p = l.pop();
		if( p == null )
			throw "<eof>";
		if( p.s )
			return makeConst(p.p);
		switch( p.p ) {
		case "(":
			var e1 = makeExpr(l);
			var p = l.pop();
			if( p == null || p.s )
				throw p.p;
			if( p.p == ")" )
				return e1;
			var e2 = makeExpr(l);
			var p2 = l.pop();
			if( p2 == null || p2.p != ")" )
				throw p2.p;
			return switch( p.p ) {
			case "+": function() { return cast e1() + e2(); };
			case "-": function() { return cast e1() - e2(); };
			case "*": function() { return cast e1() * e2(); };
			case "/": function() { return cast e1() / e2(); };
			case ">": function() { return cast e1() > e2(); };
			case "<": function() { return cast e1() < e2(); };
			case ">=": function() { return cast e1() >= e2(); };
			case "<=": function() { return cast e1() <= e2(); };
			case "==": function() { return cast e1() == e2(); };
			case "!=": function() { return cast e1() != e2(); };
			case "&&": function() { return cast e1() && e2(); };
			case "||": function() { return cast e1() || e2(); };
			default: throw "Unknown operation "+p.p;
			}
		case "!":
			var e = makeExpr(l);
			return function() {
				var v = e();
				return (v == null || v == false);
			};
		case "-":
			var e = makeExpr(l);
			return function() { return -e(); };
		}
		throw p.p;
	}

	function run( e : TemplateExpr ) {
		switch( e ) {
		case OpVar(v):
			buf.add(Std.string(resolve(v)));
		case OpExpr(e):
			buf.add(Std.string(e()));
		case OpIf(e,eif,eelse):
			var v = e();
			if( v == null || v == false ) {
				if( eelse != null ) run(eelse);
			} else
				run(eif);
		case OpStr(str):
			buf.add(str);
		case OpBlock(l):
			for( e in l )
				run(e);
		case OpForeach(e,loop):
			var v : Dynamic = e();
			try {
				if( v.hasNext == null ) {
					var x : Dynamic = v.iterator();
					if( x.hasNext == null ) throw null;
					v = x;
				}
			} catch( e : Dynamic ) {
				throw "Cannot iter on " + v;
			}
			stack.push(context);
			for( ctx in v ) {
				context = ctx;
				run(loop);
			}
			context = stack.pop();
		case OpMacro(m,params):
			var v : Dynamic = Reflect.field(macros,m);
			var pl = new Array<Dynamic>();
			var old = buf;
			pl.push(resolve);
			for( p in params ) {
				switch( p ) {
				case OpVar(v): pl.push(resolve(v));
				default:
					buf = new StringBuf();
					run(p);
					pl.push(buf.toString());
				}
			}
			buf = old;
			try {
				buf.add(Std.string(Reflect.callMethod(macros,v,pl)));
			} catch( e : Dynamic ) {
				var plstr = try pl.toString() catch( e : Dynamic ) "???";
				var msg = "Macro call "+m+"("+plstr+") failed ("+Std.string(e)+")";
				#if neko
				neko.Lib.rethrow(msg);
				#else true
				throw msg;
				#end
			}
		}
	}

}
