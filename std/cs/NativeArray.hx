/*
 * Copyright (c) 2012, The haXe Project Contributors
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
 
package cs;

@:final 
extern class NativeArray<T> implements ArrayAccess<T> {

	public function new(count : Int) : Void;

	public var length(getLength,never):Int;

	private inline function getLength() : Int {
		return untyped this.Length;
	}
	
	public inline function getElem(i : Int) : T {
		untyped return __getElemAt__(this, i);
	}
	
	public inline function setElem(i : Int, v : T) : Void {
		untyped __setElemAt__(this, i, v);
	}
}

@:final 
extern class NativeArray2<T> {

	public function new(count1 : Int, couunt2 : Int) : Void;
	
	public inline function getElem(i1 : Int, i2 : Int) : T {
		untyped return __getElemAt__(this, i1, i2);
	}
	
	public inline function setElem(i1 : Int, i2 : Int, v : T) : Void {
		untyped __setElemAt__(this, i1, i2, v);
	}	
}

@:final 
extern class NativeArray3<T> {

	public function new(count1 : Int, count2 : Int, count3 : Int) : Void;

	public inline function getElem(i1 : Int, i2 : Int, i3 : Int) : T {
		untyped return __getElemAt__(this, i1, i2, i3);
	}
	
	public inline function setElem(i1 : Int, i2 : Int, i3 : Int, v : T) : Void {
		untyped __setElemAt__(this, i1, i2, i3, v);
	}	
}

@:final 
extern class NativeJaggedArray2<T> implements ArrayAccess<NativeArray<T>> {

	public function new(count : Int) : Void;

}

@:final 
extern class NativeJaggedArray3<T> implements ArrayAccess<NativeJaggedArray2<T>> {

	public function new(count : Int) : Void;

}
