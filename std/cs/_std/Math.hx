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

/**
	This class defines mathematical functions and constants.
**/
@:core_api class Math
{
	public static var PI(default,null) : Float;
	public static var NaN(default,null) : Float;
	public static var NEGATIVE_INFINITY(default,null) : Float;
	public static var POSITIVE_INFINITY(default,null) : Float;

	public static inline function abs(v:Float):Float { 
		return untyped System.Math.Abs(v); 
	}
	
	public static inline function min(a:Float,b:Float):Float { 
		return untyped System.Math.Min(a, b);
	}
	
	public static inline function max(a:Float,b:Float):Float { 
		return untyped System.Math.Max(a, b); 
	}
	
	public static inline function sin(v:Float):Float { 
		return untyped System.Math.Sin(v); 
	}
	
	public static inline function cos(v:Float):Float {
		return untyped System.Math.Cos(v); 
	}
	
	public static inline function atan2(y:Float,x:Float):Float {
		return untyped System.Math.Atan2(y, x); 
	}
	
	public static inline function tan(v:Float):Float {
		return untyped System.Math.Tan(v); 
	}
	
	public static inline function exp(v:Float):Float {
		return untyped System.Math.Exp(v); 
	}
	
	public static inline function log(v:Float):Float {
		return untyped System.Math.Log(v); 
	}
	
	public static inline function sqrt(v:Float):Float {
		return untyped System.Math.Sqrt(v); 
	}
	
	public static inline function round(v:Float):Int {
		return untyped __int__(System.Math.Round(v)); 
	}
	
	public static inline function floor(v:Float):Int {
		return untyped __int__(System.Math.Floor(v)); 
	}
	
	public static inline function ceil(v:Float):Int {
		return untyped __int__(System.Math.Ceil(v)); 
	}
	
	public static inline function atan(v:Float):Float {
		return untyped System.Math.Atan(v); 
	}
	
	public static inline function asin(v:Float):Float {
		return untyped System.Math.Asin(v); 
	}
	
	public static inline function acos(v:Float):Float {
		return untyped System.Math.Acos(v); 
	}
	
	public static inline function pow(v:Float,exp:Float):Float {
		return untyped System.Math.Pow(v, exp); 
	}
	
	public static inline function random() : Float {
		return 0; 
	}

	public static inline function isFinite( f : Float ) : Bool {
		return untyped !System.Double.IsInfinity(f);
	}
	
	public static inline function isNaN( f : Float ) : Bool {
		return untyped System.Double.IsNaN(f);
	}

	private static function __init__() : Void untyped {
		untyped {
			Math.PI = System.Math.PI;
			Math.NaN = System.Double.NaN;
			Math.NEGATIVE_INFINITY = System.Double.NegativeInfinity;
			Math.POSITIVE_INFINITY = System.Double.PositiveInfinity;
		}
	}

}


