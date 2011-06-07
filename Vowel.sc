//Vowel implememted jointly by Florian Grond and Till Bovermann june 2011
//
//This class implementation is supported by:
//the Ambient Intelligence groupe CITEC  http://www.techfak.uni-bielefeld.de/ags/ami  Bielefeld University
//the Department of http://TAI-Studio.org, Media, Aalto University, Helsinki.
//
//thanx to Alberto deCampop and and Julian Rohrhuber


// the in the formant library are taken from the Csoundmanual:
// http://ecmc.rochester.edu/onlinedocs/Csound/Appendices/table3.html
// \vowel,\register, formant frequecies, formant dB, formant width


// changes to Vowel
// 18.02.2010 fg: added method save and load
// 18.02.2010 fg: added method addFormant and removeFormant
// 18.02.2010 fg: added method midinote rqs amps
// 18.02.2010 fg: added method plot
// 18.02.2010 fg: added method ampAt
// 18.02.2010 fg: added method brightenCAmpSum

// Till please look at compose, did not get it to work with midinotes for linear combination


// changes to Vowel
// 6.2.2010 tb: added asEvent
// 6.2.2010 tb: moved initialization of formLib from iniClass to the actual place where it is used first
// 6.2.2010 tb: added channel expansion, so you now can write Vowel([\a, \e], [\bass, \soprano])

// changes to Formants
// 6.2.2010 tb: added a new implementation that should be preferred to Formants:
//	+ got rid of explicit use of freqs/amps/widths in Formants as it is not really used
//	+ added channel expansion
//	+ added unfold to change between summing or not summing the formants together...
//	+ added mods parameter for freqs/widths/amps

Vowel {
	classvar formLib;
	var <>freqs, <>dBs, <>widths;

	// evaluated the first time you use it
	*initLib{
	// if (formLib.isNil, {	
		formLib = Library.new;
		formLib	
			.put( 'a', 'soprano', 'freq',[ 800, 1150, 2900, 3900, 4950 ])
			.put( 'a', 'soprano', 'db', [ 0, -6, -32, -20, -50 ])
			.put( 'a', 'soprano', 'bw',	 [ 80, 90, 120, 130, 140 ])
	
			.put( 'e', 'soprano', 'freq',[ 350, 2000, 2800, 3600, 4950 ])
			.put( 'e', 'soprano', 'db', [ 0, -20, -15, -40, -56 ])
			.put( 'e', 'soprano', 'bw',	 [ 60, 100, 120, 150, 200 ])
	
			.put( 'i', 'soprano', 'freq',[270, 2140, 2950, 3900, 4950])
			.put( 'i', 'soprano', 'db', [0, -12, -26, -26, -44])
			.put( 'i', 'soprano', 'bw',	 [60, 90, 100, 120, 120])
	
			.put( 'o', 'soprano', 'freq',[450, 800, 2830, 3800, 4950])
			.put( 'o', 'soprano', 'db', [0, -11, -22, -22, -50])
			.put( 'o', 'soprano', 'bw',	 [70, 80, 100, 130, 135])
	
			.put( 'u', 'soprano', 'freq',[325, 700, 2700, 3800, 4950])
			.put( 'u', 'soprano', 'db', [0, -16, -35, -40, -60])
			.put( 'u', 'soprano', 'bw',	 [50, 60, 170, 180, 200])
	
	
			.put( 'a', 'alto', 'freq',	[800, 1150, 2800, 3500, 4950])
			.put( 'a', 'alto', 'db',	[0, -4, -20, -36, -60])
			.put( 'a', 'alto', 'bw',	[80, 90, 120, 130, 140])
	
			.put( 'e', 'alto', 'freq',	[400, 1600, 2700, 3300, 4950])
			.put( 'e', 'alto', 'db',	[0, -24, -30, -35, -60])
			.put( 'e', 'alto', 'bw',	[60, 80, 120, 150, 200])
	
			.put( 'i', 'alto', 'freq',	[350, 1700, 2700, 3700, 4950])
			.put( 'i', 'alto', 'db',	[0, -20, -30, -36, -60])
			.put( 'i', 'alto', 'bw',	[50, 100, 120, 150, 200])
	
			.put( 'o', 'alto', 'freq',	[450, 800, 2830, 3500, 4950])
			.put( 'o', 'alto', 'db',	[0, -9, -16, -28, -55])
			.put( 'o', 'alto', 'bw',	[70, 80, 100, 130, 135])
	
			.put( 'u', 'alto', 'freq',	[325, 700, 2530, 3500, 4950])
			.put( 'u', 'alto', 'db',	[0, -12, -30, -40, -64])
			.put( 'u', 'alto', 'bw',	[50, 60, 170, 180, 200])
	
	
			.put( 'a', 'tenor', 'freq',	[650, 1080, 2650, 2900, 3250])
			.put( 'a', 'tenor', 'db',	[0, -6, -7, -8, -22])
			.put( 'a', 'tenor', 'bw',	[80, 90, 120, 130, 140])
	
			.put( 'e', 'tenor', 'freq',	[400, 1700, 2600, 3200, 3580])
			.put( 'e', 'tenor', 'db',	[0, -14, -12, -14, -20])
			.put( 'e', 'tenor', 'bw',	[70, 80, 100, 120, 120])
	
			.put( 'i', 'tenor', 'freq',	[290, 1870, 2800, 3250, 3540])
			.put( 'i', 'tenor', 'db',	[0, -15, -18, -20, -30])
			.put( 'i', 'tenor', 'bw',	[40, 90, 100, 120, 120])
	
			.put( 'o', 'tenor', 'freq',	[400, 800, 2600, 2800, 3000])
			.put( 'o', 'tenor', 'db',	[0, -10, -12, -12, -26])
			.put( 'o', 'tenor', 'bw',	[40, 80, 100, 120, 120])
	
			.put( 'u', 'tenor', 'freq',	[350, 600, 2700, 2900, 3300])
			.put( 'u', 'tenor', 'db',	[0, -20, -17, -14, -26])
			.put( 'u', 'tenor', 'bw',	[40, 60, 100, 120, 120])
		
	
			.put( 'a', 'bass', 'freq',	[600, 1040, 2250, 2450, 2750])
			.put( 'a', 'bass', 'db',	[0, -7, -9, -9, -20])
			.put( 'a', 'bass', 'bw',	[60, 70, 110, 120, 130])
	
			.put( 'e', 'bass', 'freq',	[400, 1620, 2400, 2800, 3100])
			.put( 'e', 'bass', 'db',	[0, -12, -9, -12, -18])
			.put( 'e', 'bass', 'bw',	[40, 80, 100, 120, 120])
	
			.put( 'i', 'bass', 'freq',	[250, 1750, 2600, 3050, 3340])
			.put( 'i', 'bass', 'db',	[0, -30, -16, -22, -28])
			.put( 'i', 'bass', 'bw',	[60, 90, 100, 120, 120])
	
			.put( 'o', 'bass', 'freq',	[400, 750, 2400, 2600, 2900])
			.put( 'o', 'bass', 'db',	[0, -11, -21, -20, -40])
			.put( 'o', 'bass', 'bw',	[40, 80, 100, 120, 120])
	
			.put( 'u', 'bass', 'freq',	[350, 600, 2400, 2675, 2950])
			.put( 'u', 'bass', 'db',	[0, -20, -32, -28, -36])
			.put( 'u', 'bass', 'bw',	[40, 80, 100, 120, 120])
	
	
			.put( 'a', 'counterTenor', 'freq',	[660, 1120, 2750, 3000, 3350])
			.put( 'a', 'counterTenor', 'db',		[0, -6, -23, -24, -38])
			.put( 'a', 'counterTenor', 'bw',		[80, 90, 120, 130, 140])
	
			.put( 'e', 'counterTenor', 'freq',	[440, 1800, 2700, 3000, 3300])
			.put( 'e', 'counterTenor', 'db',		[0, -14, -18, -20, -20])
			.put( 'e', 'counterTenor', 'bw',		[70, 80, 100, 120, 120])
	
			.put( 'i', 'counterTenor', 'freq',	[270, 1850, 2900, 3350, 3590])
			.put( 'i', 'counterTenor', 'db',		[0, -24, -24, -36, -36])
			.put( 'i', 'counterTenor', 'bw',		[40, 90, 100, 120, 120])
	
			.put( 'o', 'counterTenor', 'freq',	[430, 820, 2700, 3000, 3300])
			.put( 'o', 'counterTenor', 'db',		[0, -10, -26, -22, -34])
			.put( 'o', 'counterTenor', 'bw',		[40, 80, 100, 120, 120])
	
			.put( 'u', 'counterTenor', 'freq',	[370, 630, 2750, 3000, 3400])
			.put( 'u', 'counterTenor', 'db',		[0, -20, -23, -30, -34])
			.put( 'u', 'counterTenor', 'bw',		[40, 60, 100, 120, 120]);		// }
		//) // end if
	}
	
	save { |path = nil, timbre = \mytimbre, register = \myregister|
		var file;
		file = File.new(path, "a+");
		
		file.write("Vowel.formLib.put("++" \\"++timbre++","++" \\"++register++", \\freq ,"++this.freqs.asString++" );\n");
		file.write("Vowel.formLib.put("++" \\"++timbre++","++" \\"++register++", \\db ,"++this.dBs.asString++" );\n");
		file.write("Vowel.formLib.put("++" \\"++timbre++","++" \\"++register++", \\bw ,"++this.widths.asString++" );\n");
		file.write("\"loaded "++timbre++", "++register++", saved on"+Date.localtime.format("%Yy%mm%dd%Hh%Mm%Ss").asString+"\".postln;\n");
		file.write("\n\n");
		
		file.close;		
	}

	*load {|path|
	var file;
	file = File.open(path, "r");
	file.readAllString.interpret;
	file.close;
	}

	*formLib {
		formLib.isNil.if{this.initLib};
		^formLib;	
	}

	*new1 {|vowel = \a, register= \bass|
		var data;
		
		// is there a formlib already? otherwise create it
		formLib.isNil.if{this.initLib};

		data = Vowel.formLib[vowel, register];
		data = [\freq, \db, \bw].collect{|id| data[id]};
		^this.basicNew(*data)
	}

	*new {|vowel = \a, register= \bass|
		^[vowel, register].flop.collect{|args|
			this.new1(*args);
		}.unbubble
	}

	*basicNew {|freqs, dBs, widths|
		^super.new.init(freqs, dBs, widths)	
	}

	init {|argFreqs, argDBs, argWidths|
		freqs = argFreqs.copy;
		dBs = argDBs.copy;
		widths = argWidths.copy;
	}
	
	midinotes {
		^this.freqs.cpsmidi
	}
	
	amps {
		^this.dBs.dbamp
	}
	
	rqs {
		^this.widths.reciprocal
	}
	
	addFormants { |freq = 0, db = 1, width = -100|
		[freq,db,width].flop.collect({|form| this.freqs.add(form[0]); this.dBs.add(form[1]); this.widths.add(form[2]);})
	}
	
	removeFormants { |index|
	index.reverseDo({|item,i|  this.freqs.removeAt(item); this.dBs.removeAt(item); this.widths.removeAt(item); })
	}	
		
	ampAt {|freq, filterOrder = 1|
		// var half = 0.5.ampdb;
		var half = -6.0205999132796;

		^[this.freqs,this.dBs,this.widths, filterOrder].flop.collect({|form|  
			( 
				form[1] 
				+ ( (( (freq - form[0]).abs / (form[2] * 0.5) ).pow(form[3]) ) * half
				) 
			).dbamp }).sum
	}
	
	
	plot {|fmin = 0, fmax = 6000, fstep = 2, order = 1|
	var range = {|i| (i*fstep) + fmin}!((fmax - fmin) / fstep);
	^this.ampAt(range, order).ampdb.plot("dB (-100 to 0) / frequency ("++fmin.asString++" to "++fmax.asString++" Hz)", minval: -100, maxval: 0)
	}
	
	asArray { 
		^[freqs, dBs, widths]
	}
	
	asEvent {
		^(
			freqs: freqs,
			dBs: dBs,
			widths: widths
		)	
	}
	
	asKeyValuePairs {|id = ""|
		^this.asEvent.asKeyValuePairs.clump(2).collect{|pair|
			[(pair[0] ++ id).asSymbol,pair[1]]	
		}.flatten
	}
	
	asFlatArray { 
"Vowel:asFlatArray is deprecated.".inform;
		^this.asArray.flat
	}

	addControls {|id = "", rate = \kr|
		var pairs, result;
		 
		result = this.class.basicNew;

		pairs = this.asKeyValuePairs.clump(2);
		pairs.collect{|pair, i|
			//pair.postln;
			result.perform(pair[0].asSetter, (pair[0] ++ id).asSymbol.perform(rate, pair[1]));
		};
		
		^result
	}

	// math support
	+ { arg that, adverb; ^that.performBinaryOpOnVowel('+', this, adverb) }
	- { arg that, adverb; ^that.performBinaryOpOnVowel('-', this, adverb) }
	* { arg that, adverb; ^that.performBinaryOpOnVowel('*', this, adverb) }
	/ { arg that, adverb; ^that.performBinaryOpOnVowel('/', this, adverb) }


	performBinaryOpOnVowel {|aSelector, aVowel|
		var that = this.class.new;
		
		that.freqs 	= this.freqs		.perform(aSelector, that.freqs);
		that.dBs 		= this.dBs	 	.perform(aSelector, that.dBs);
		that.widths 	= this.widths		.perform(aSelector, that.widths);
		^that		
	}

	performBinaryOpOnSimpleNumber { arg aSelector, aNumber; 
		^this.class.basicNew(*this.asArray.collect{ arg item; 
			aNumber.perform(aSelector, item)
		}) 
	}
	
	
	// blendFraq should be either a number, or an array [freqFrac, dbFrac, widthFrac]
	blend{|that, blendFrac=0.5| 
		var fracs = blendFrac.asArray; // make sure it's an array;
		
		//  use wrapAt to access correct frac-value
		^this.class.basicNew(
			blend(this.midinotes,  that.midinotes, fracs.wrapAt(0)).midicps,
			blend(this.dBs,    that.dBs, fracs.wrapAt(1)),
			blend(this.widths, that.widths, fracs.wrapAt(2))
		)
	}

/*	blend (that, freqFrac, ampFrac, widthFrac)
	
		blend2 allows you to blend two vowels with individual coefficients for freq, amp, width. The blending is a linear interpolation between the arrays freqs, widths and dBs.
		that - Vowel
		freqFrac - frequency coefficient. Default value is 0.5. range from 0.0 to 1.0
		ampFrac - amplitude coefficient. Default value is 0.5. range from 0.0 to 1.0
		widthFrac - bandwidth coefficient. Default value is 0.5. range from 0.0 to 1.0
		
		~v1 = Vowel(\a, \bass)
		~v2 = Vowel(\i, \soprano)
		
		~v1.blend1(~v2, 0)
		~v1.blend1(~v2, 0.5)
		~v1.blend1(~v2, 0.1, 0.5, 0.8)

		in the following example you''l notice the the biggest contributioin to the recognizability of a vowel are first the frequencies (MouseX) and second the amplitudes (MouseY).  
		The bandwidths do not contribute much (SinOsc):

		{ Formants.ar(150,  Vowel(\a, \bass).blend1(Vowel(\u, \bass), MouseX.kr(0,1), MouseY.kr(0,1), SinOsc.kr(0.5, 0, 0.5, 0.5) ) ) * 0.1  }.play
		{ Formants.ar(150,  Vowel(\e, \bass).blend1(Vowel(\i, \bass), MouseX.kr(0,1), MouseY.kr(0,1), SinOsc.kr(0.5, 0, 0.5, 0.5) ) ) * 0.1  }.play
		{ Formants.ar(150,  Vowel(\i, \bass).blend1(Vowel(\o, \bass), MouseX.kr(0,1), MouseY.kr(0,1), SinOsc.kr(0.5, 0, 0.5, 0.5) ) ) * 0.1  }.play
*/

/*	blendOneFac {|that, blendFrac=0.5|
		var vowelBlend;
		vowelBlend = this.class.new;

		vowelBlend.freqs 	= blend(this.midinotes,  that.midinotes, blendFrac).midicps; 
		vowelBlend.dBs 	= blend(this.dBs,    that.dBs, blendFrac); 
		vowelBlend.widths 	= blend(this.widths, that.widths, blendFrac); 

		^vowelBlend;
	}	

	blendThreeFac {|that, freqFrac = 0.5, dbFrac = 0.5, widthFrac = 0.5|
		var vowelBlend;
		vowelBlend = this.class.new;

		vowelBlend.freqs 	= blend(this.midinotes,  that.midinotes, freqFrac).midicps; 
		vowelBlend.dBs 	= blend(this.dBs,    that.dBs, dbFrac); 
		vowelBlend.widths 	= blend(this.widths, that.widths, widthFrac); 

		^vowelBlend;
	}
*/

	brightenRel {|bright = 1, refFormant = 0|
		var refFormat_dB = this.dBs[refFormant];
		this.dBs =  (this.dBs * bright);
		this.dBs = this.dBs - (this.dBs[refFormant] - refFormat_dB);
		^this
	}
		
	brightenLin {|bright = 1, refFormant = 0|
		var refFormat_dB = this.dBs[refFormant];
		this.dBs =  this.dBs + (bright * this.freqs.log);
		this.dBs = this.dBs - (this.dBs[refFormant] - refFormat_dB);
		^this
	}
	
	brightenExp {|bright = 1|
		var ampssum = (this.amps).sum;
		this.dBs =  ( this.amps).pow(bright) ;
		this.dBs =  (this.dBs * (ampssum / this.dBs.sum)).ampdb;
		^this
	}

	*compose {|vowelNames, registers, weights|
		var all;
		var vowel, vowelName, register;
		var midinotes = 0, dBs = 0, widths = 0;
		
		weights = weights.asArray;
		
		// create individual vowels convert freqs to midinotes
		all = [vowelNames, registers].flop.collect{|args, i|
			# vowelName, register = args;
			vowel = Vowel(vowelName, register);

			midinotes = midinotes + (vowel.midinotes * weights.wrapAt(i));
			dBs = dBs + (vowel.dBs * weights.wrapAt(i));
			widths = widths + (vowel.widths * weights.wrapAt(i));		};
		
		^Vowel.basicNew(
			midinotes.midicps, 
			dBs,
			widths
		);	
	}
		
	asKlankRef {|baseFreq = 440, numPartials = 13, ring =  1|
		var pList, gList;
				
	"Vowel:asKlankRef : deprecated, please use ampAt or dbAt to construct your own KlankRef Argument".inform;
				
		pList = {|i| baseFreq * (i+1)    }!numPartials;
		gList = freqs.collect{|freq, i| 
			((pList - freq).abs * (-1 / widths[i])).exp 
				* this.dBs[i].dbamp
		}.sum;

		^(`([pList, gList, gList * [ring]]))
	}


	printOn { arg stream;
		var title;
		stream << this.class.name << ".basicNew";
		this.storeParamsOn(stream);
	}
	
	storeArgs { ^this.asArray }
}

// Here you find some convenience Pseudo Ugen Classes,
// to be used together with Vowel

Formants {
	*ar1 {|baseFreq = 100, vowel, freqMods = 1, ampMods = 1, widthMods = 1, unfold = false|
		var freqs, dBs, amps, widths;
		var out; 
		
		#freqs, dBs, widths = vowel.asArray;
	
		freqs = freqs * freqMods;
		amps = dBs.dbamp * ampMods;
		widths = widths * widthMods;
	
		out = [freqs, widths, amps].flop.collect{ |args| 
			Formant.ar(baseFreq, *args); 
		}.flop;
		
		// don't mix if unfold is true
		^(unfold.if({ out; },{ out.collect(_.sum); })).unbubble; 
		// unbubble for correct channel expansion behaviour, if only one baseFreq is given
	}
	
	*ar {|baseFreq = 100, vowel, freqMods = 1, ampMods = 1, widthMods = 1, unfold = false|
		^[baseFreq, vowel, [freqMods], [ampMods], [widthMods]].flop.collect({ |inputs|
			this.ar1(*(inputs ++ unfold));
		}).unbubble;		
	}
}


/*
Formants1 {
	*ar1 {|baseFreq = 100, vowel, freqMods = 1, ampMods = 1, widthMods = 1, unfold = false|
		var freqs, dBs, amps, widths;
		var out; 
		
		#freqs, dBs, widths = vowel.asArray;
	
		freqs = freqs * freqMods;
		amps = dBs.dbamp * ampMods;
		widths = widths * widthMods;
	
		out = [baseFreq, freqs, widths, dBs].flop.collect{ |args| 
			Formant.ar(args[0], args[1], args[2], args[3].dbamp) 
		};

		// don't mix if unfold is true
		^unfold.if({out},{out.sum})
	}
	
	*ar {|baseFreq = 100, vowel, freqMods = 1, ampMods = 1, widthMods = 1, unfold = false|
		^[baseFreq, vowel, [freqMods], [ampMods], [widthMods]].flop
			.postcs
		.collect({ |inputs|
			this.ar1(*(inputs ++ unfold));
		}).unbubble;	
	}
}
*/

BPFStack {
	*ar1 {|in, vowel, freqMods = 1, ampMods = 1, widthMods = 1, unfold = false|
		var freqs, dBs, amps, widths;
		var out; 
		
		#freqs, dBs, widths = vowel.asArray;
	
		freqs = freqs * freqMods;
		amps = dBs.dbamp * ampMods;
		widths = (widths * widthMods).reciprocal;
	
		out = [freqs, widths, amps].flop.collect{ |args| 
			BPF.ar(in, *args); 
		}.flop;
		
		// don't mix if unfold is true
		^(unfold.if({ out; },{ out.collect(_.sum); })).unbubble; 
		// unbubble for correct channel expansion behaviour, if only one baseFreq is given
	}
	
	*ar {|in, vowel, freqMods = 1, ampMods = 1, widthMods = 1, unfold = false|
		^[in, vowel, [freqMods], [ampMods], [widthMods]].flop.collect({ |inputs|
			this.ar1(*(inputs ++ unfold));
		}).unbubble;		
	}
}


+ Object {
	
	performBinaryOpOnVowel{|aSelector, aVowel|

		^aVowel.class.basicNew(*aVowel.asArray.collect({ arg item; 
			this.perform(aSelector, item)
		})) 
}
	
}

/*
Helper(Vowel, Document.current.path.copyRange(0, Document.current.path.size-4)++"_.html")
*/