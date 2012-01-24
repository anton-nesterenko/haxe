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

import ArrayIterator;

@:core_api @:final @:native_base("System.Collections.Generic.List<T>") 
class Array<T> {

	public function new() : Void {
	}

	public var length(default,null):Int;

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
		return new ArrayIterator<T>(this);
	}

	public function insert( pos : Int, x : T ) : Void {
		untyped {
			this.Insert(pos, x);
			length = this.Count;
		}
	}

	public function join( sep : String ) : String {
		return null;
	}

	public function toString() : String {
		var ret : String = null;
		untyped __cs__('
			var sb = new System.Text.StringBuilder();
			sb.Append("[");
			int l = Count;
			for (int i = 0; i < l; i++) {
				if (i > 0) {
					sb.Append(",");
				}
				sb.Append(this[i].ToString());
			}
			sb.Append("]");
			ret = sb.ToString();
		');
		return ret;
	}

	public function pop() : Null<T> {
		untyped {
			if (this.Count > 0) {
				var ret : Null<T> = this[this.Count - 1];
				this.RemoveAt(this.Count - 1);
				length--;
				return ret;
			} else {
				return null;
			}
		}
	}

	public function push(x:T) : Int {
		untyped {
			this.Add(x);
			length++;
			return length - 1;
		}
	}

	public function unshift(x : T) : Void {
		untyped {
			this.Insert(0, x);
			length++;
		}
	}

	public function remove(x : T) : Bool {
		untyped {
			var ret : Bool = this.Remove(x);
			if (ret) {
				length--;
			}
			return ret;
		}
	}

	public function reverse() : Void {
		untyped this.Reverse();
	}

	public function shift() : Null<T> {
		untyped {
			if (this.Count > 0) {
				var ret : Null<T> = this[0];
				this.RemoveAt(0);
				length--;
				return ret;
			} else {
				return null;
			}
		}
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

