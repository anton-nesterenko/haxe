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

@:core_api @:final @:native("string") 
extern class String {

	public var length(getLength,null) : Int;

	private inline function getLength() : Int {
		return untyped this.Length;
	}

	public function new(s:String) : Void;

	public inline function charAt(index:Int) : String {
		untyped return this[index].ToString();
	}

	public inline function charCodeAt(index : Int) : Null<Int> {
		untyped return index < s.Length ? __int__(s[index]) : null;
	}

	public inline function indexOf( str : String, ?startIndex : Int ) : Int {
		return untyped this.IndexOf(str, startIndex != null ? __int__(startIndex) : 0);
	}

	public inline function lastIndexOf( str : String, ?startIndex : Int ) : Int {
		return untyped this.IndexOf(str, startIndex != null ? __int__(startIndex) : 0);
	}

	public inline function split( delimiter : String ) : Array<String> {
		untyped return this.Split(delimiter);
	}

	public inline function substr( pos : Int, ?len : Int ) : String {
		untyped return this.Substring(pos, len);
	}

	public inline function toLowerCase() : String {
		untyped return this.ToLower();
	}

	public inline function toUpperCase() : String {
		untyped return this.ToUpper();
	}

	public inline function toString() : String {
		return this;
	}

	public static function fromCharCode( code : Int ) : String untyped {
		return __char__(code).ToString();
	}

}