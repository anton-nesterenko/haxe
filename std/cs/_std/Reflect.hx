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

@:core_api class Reflect {

	public static function hasField( o : Dynamic, field : String ) : Bool untyped {
		return null;
	}

	public static function field( o : Dynamic, field : String ) : Dynamic untyped {
		return null;
	}

	public inline static function setField( o : Dynamic, field : String, value : Dynamic ) : Void untyped {
	}

	public static inline function getProperty( o : Dynamic, field : String ) : Dynamic {
		return null;
	}

	public static inline function setProperty( o : Dynamic, field : String, value : Dynamic ) : Void {
	}
	
	public inline static function callMethod( o : Dynamic, func : Dynamic, args : Array<Dynamic> ) : Dynamic untyped {
		return null;
	}

	public static function fields( o : Dynamic ) : Array<String> untyped {
		return null;
	}

	public static function isFunction( f : Dynamic ) : Bool untyped {
		return false;
	}

	public static function compare<T>( a : T, b : T ) : Int {
		return 0;
	}

	public static function compareMethods( f1 : Dynamic, f2 : Dynamic ) : Bool {
		return false;
	}

	public static function isObject( v : Dynamic ) : Bool untyped {
		return false;
	}

	public static function deleteField( o : Dynamic, f : String ) : Bool untyped {
		return false;
	}

	public static function copy<T>( o : T ) : T {
		return null;
	}

	public static function makeVarArgs( f : Array<Dynamic> -> Dynamic ) : Dynamic {
		return null;
	}

}
