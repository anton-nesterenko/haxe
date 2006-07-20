/*
 * Copyright (c) 2006, Motion-Twin
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
 * THIS SOFTWARE IS PROVIDED BY MOTION-TWIN "AS IS" AND ANY
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
package mtwin.mail.imap;

signature Flags = Array<String>

enum Section {
	Flags;
	Uid;
	BodyStructure;
	Envelope;
	InternalDate;
	Body(ss:BodySection);
	BodyPeek(ss:BodySection);
}

enum BodySection {
	Header;
	Mime;
	Text;
	SubSection(id:String,ss:BodySection);
}

enum Collection {
	Single(i:Int);
	Range(s:Int,e:Int);
	Composite(l:Array<Collection>);
}

class Tools {
	public static function listToColl( l : List<Int> ) : Collection {
		var a = new Array();
		for( e in l )
			a.push( Single(e) );
		return Composite(a);
	}

	public static function collString( r : Collection ) : String {
		return switch( r ){
			case Single(i): Std.string(i);
			case Range(s,e): Std.string(s)+":"+Std.string(e);
			case Composite(l):
				var t = new List<String>();
				for( e in l )
					t.add(collString(e));
				t.join(",");
		}
	}

	public static function sectionString( a : Array<Section> ) : String{
		var r = new List();
		
		if( a == null || a.length < 1 )
			return "";
		
		for( s in a ){
			r.add( switch( s ){
				case Flags: "FLAGS";
				case Uid: "UID";
				case BodyStructure: "BODYSTRUCTURE";
				case Envelope: "ENVELOPE";
				case InternalDate: "INTERNALDATE";
				case Body(ss): "BODY["+bodySectionString(ss)+"]";
				case BodyPeek(ss): "BODY.PEEK["+bodySectionString(ss)+"]";
					
			});
		}
		return "("+r.join(" ")+")";
	}

	static function bodySectionString( ss : BodySection ){
		if( ss == null )
			return "";

		return switch( ss ){
			case Text: "TEXT";
			case Header: "HEADER";
			case Mime: "MIME";
			case SubSection(id,nss):
				var t = bodySectionString(nss);
				if( id == null || id == "" ) 
					t;
				else if( t == "" )
					id;
				else
					id+"."+t;
		}
	}

}
