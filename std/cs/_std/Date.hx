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
	The Date class is used for date manipulation. There is some extra functions
	available in the [DateTools] class.
**/

@:core_api class Date
{
	/**
		Creates a new date object.
	**/
	public function new(year : Int, month : Int, day : Int, hour : Int, min : Int, sec : Int ) : Void {
	}

	/**
		Returns the timestamp of the date. It's the number of milliseconds
		elapsed since 1st January 1970. It might only have a per-second precision
		depending on the platforms.
	**/
	public function getTime() : Float {
		return 0;
	}

	/**
		Returns the hours value of the date (0-23 range).
	**/
	public function getHours() : Int {
		return 0;
	}

	/**
		Returns the minutes value of the date (0-59 range).
	**/
	public function getMinutes() : Int {
		return 0;
	}

	/**
		Returns the seconds of the date (0-59 range).
	**/
	public function getSeconds() : Int {
		return 0;
	}

	/**
		Returns the full year of the date.
	**/
	public function getFullYear() : Int {
		return 0;
	}

	/**
		Returns the month of the date (0-11 range).
	**/
	public function getMonth() : Int {
		return 0;
	}

	/**
		Returns the day of the date (1-31 range).
	**/
	public function getDate() : Int {
		return 0;
	}

	/**
		Returns the week day of the date (0-6 range).
	**/
	public function getDay() : Int {
		return 0;
	}

	/**
		Returns a string representation for the Date, by using the
		standard format [YYYY-MM-DD HH:MM:SS]. See [DateTools.format] for
		other formating rules.
	**/
	public function toString():String {
		return "";
	}

	/**
		Returns a Date representing the current local time.
	**/
	public static function now() : Date {
		return null;
	}

	/**
		Returns a Date from a timestamp [t] which is the number of
		milliseconds elapsed since 1st January 1970.
	**/
	public static function fromTime( t : Float ) : Date {
		return null;
	}

	/**
		Returns a Date from a formated string of one of the following formats :
		[YYYY-MM-DD hh:mm:ss] or [YYYY-MM-DD] or [hh:mm:ss]. The first two formats
		are expressed in local time, the third in UTC Epoch.
	**/
	public static function fromString( s : String ) : Date {
		return null;
	}
}

