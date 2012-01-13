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

@native("System.Collections.Generic.List<T>.Enumerator")
extern class Enumerator<T> {
}

/* @:final
class ListIterator<T> {

	private var _e : Enumerator<T>;
	private var _hasNext : Bool;
	
	public function new(e : Enumerator<T>) {
		_e = e;
		untyped _hasNext = _e.MoveNext();
	}
	
    function hasNext() : Bool {
    	return _hasNext;
    }
    
    function next() : T {
    	var c : T;
    	untyped {
    		c = _e.Current;
    		_hasNext = _e.MoveNext();
    	}
    	return c;
    }
} */

@:core_api @:final @:native_base("System.Collections.Generic.List<T>") 
class Array<T> {

	public function new() : Void {
	}

	public var length(default,null):Int;

	private function getLength() : Int {
		untyped return this.Length;
	}
	
	private function setLength(l : Int) : Int {
		return 0;
	}

	public function concat( a : Array<T>) : Array<T> {
		var a2 = new Array<T>();
		untyped {
			a2.AddRange(this);
			a2.AddRange(a);
		}
		return a2;
	}

	public function copy() : Array<T> {
		var a = new Array<T>();
		untyped {
			a.AddRange(this);
		}
		return a;
	}

	public function iterator() : Iterator<Null<T>> {
		untyped return null;
	}

	public function insert( pos : Int, x : T ) : Void {
		untyped this.Insert(pos, x);
	}

	public function join( sep : String ) : String {
		return null;
	}

	public function toString() : String {
		var s = new StringBuf();
		s.add("[");
		var it = iterator();
		for( i in it ) {
			s.add(i);
			if( it.hasNext() )
				s.add(", ");
		}
		s.add("]");
		return s.toString();
	}

	public function pop() : Null<T> {
		return null;
	}

	public function push(x:T) : Int {
		untyped this.Add(x);
		return untyped this.Length - 1;
	}

	public function unshift(x : T) : Void {
	}

	public function remove(x : T) : Bool {
		untyped return this.Remove(x);
	}

	public function reverse() : Void {
		untyped this.Reverse();
	}

	public function shift() : Null<T> {
		return null;
	}

	public function slice( pos : Int, ?end : Int ) : Array<T> {
		return null;
	}

	public function sort(f:T->T->Int) : Void {
	}

	public function splice( pos : Int, len : Int ) : Array<T> {
		return null;
	}
}
