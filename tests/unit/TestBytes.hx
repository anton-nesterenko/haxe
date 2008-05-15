﻿package unit;

class TestBytes extends Test {

	function test() {
		var b = Bytes.alloc(10);
		eq(b.length,10);
		// get-set
		for( i in 0...10 )
			eq(b.get(i),0);
		unspec(function() b.get(-1));
		unspec(function() b.get(11));
		b.set(1,20);
		eq(b.get(1),20);
		unspec(function() b.set(-1,20));
		unspec(function() b.set(11,20));
		unspec(function() b.set(0,1000));
		// ofString
		var b2 = Bytes.ofString("ABCD");
		eq(b2.length,4);
		eq(b2.get(0),"A".charCodeAt(0));
		eq(b2.get(1),"B".charCodeAt(0));
		eq(b2.get(2),"C".charCodeAt(0));
		eq(b2.get(3),"D".charCodeAt(0));
		var b3 = Bytes.ofString("é");
		eq(b3.length,2);
		eq(b3.get(0),0xC3);
		eq(b3.get(1),0xA9);
		// blit
		b.blit(3,b2,1,3);
		eq(b.get(2),0);
		eq(b.get(3),"B".charCodeAt(0));
		eq(b.get(4),"C".charCodeAt(0));
		eq(b.get(5),"D".charCodeAt(0));
		eq(b.get(6),0);
		exc(function() b.blit(-1,b2,1,3));
		exc(function() b.blit(0,b2,2,3));
		exc(function() b.blit(9,b2,1,3));
		exc(function() b.blit(0,b2,-1,3));
		exc(function() b.blit(0,b2,1,-1));
		exc(function() b.blit(0,b2,1,20));
		// forward
		b.blit(4,b,3,3);
		eq(b.get(2),0);
		eq(b.get(3),"B".charCodeAt(0));
		eq(b.get(4),"B".charCodeAt(0));
		eq(b.get(5),"C".charCodeAt(0));
		eq(b.get(6),"D".charCodeAt(0));
		eq(b.get(7),0);
		// backward
		b.blit(3,b,5,3);
		eq(b.get(2),0);
		eq(b.get(3),"C".charCodeAt(0));
		eq(b.get(4),"D".charCodeAt(0));
		eq(b.get(5),0);
		eq(b.get(6),"D".charCodeAt(0));
		eq(b.get(7),0);
		// readString
		var bs = Bytes.ofString("One é accent");
		bs.set(3,0); // cut betwen "One" and "é"
		eq(bs.readString(0,3),"One");
		eq(bs.readString(4,bs.length-4),"é accent");
		eq(bs.readString(4,2),"é");
		exc(function() bs.readString(-1,1));
		exc(function() bs.readString(1,20));
		unspec(function() bs.readString(4,1)); // might not allow to cut UTF8 char
		unspec(function() bs.readString(1,5)); // the handling of \0 might vary
		/**
		 	HANDLING of 0x00 in string :
				Flash8 : ignore
				Flash9 : cut string
				JS/FFOX, JS/IE7, Neko : ok (\0 displayed as ? on Firefox, string cut on IE7)
				JS/IE6 : todo
				JS/Safari : todo
				JS/Opera : todo
		**/
		// toString
		eq(b2.toString(),"ABCD");
		// compare
		var strings = ["ABCD","ABDC","ABCDE","ABC","BC","AAAAAAAAA",
			#if !flash8 "" #end // there is an error with empty string comparison in Flash8
		];
		for( s1 in strings )
			for( s2 in strings ) {
				var c = Bytes.ofString(s1).compare(Bytes.ofString(s2));
				infos("compare "+s1+" and "+s2);
				eq( c < 0, s1 < s2 );
				eq( c > 0, s1 > s2 );
				eq( c == 0, s1 == s2 );
			}
		infos(null);
	}

}
