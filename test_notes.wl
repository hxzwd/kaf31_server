
TNOTE := Function[{}, Get["test_notes.wl"]];


(*
coeffs = {
		{A3 -> 120},
		{A2 -> (-12*A3*sigma)/(-24 + A3)},
		{A1 -> (-A2^2 + 6*A3 - 228*A3*b - 12*A2*sigma)/(2*(-6 + A3))}
	};
*)

coeffs = {
		{A3 -> 120},
		{A2 -> (-12*A3*sigma)/(-24 + A3)},
		{A1 -> (-A2^2 + 6*A3 - 228*A3*b - 12*A2*sigma)/(2*(-6 + A3))},
		{A0 -> (2*A2 - A1*A2 - 40*A2*b + A3*C0 - 2*A1*sigma + 18*A3*b*sigma)/A3}
	};

ALG[coeffs_] := Module[ { res, currA, tailB, B, resB, R },

	res = { };

	If[ Length[coeffs] == 1,
		res = Map[({ #1 })&, coeffs[[1]]];
		Return[res]
	];


	For[ i = 1, i <= Length[coeffs[[1]]], i++,
	
		currA = coeffs[[1]][[i]];
		tailB = coeffs[[2;;]];

		B = tailB/.currA;
		Print[B];
		resB = ALG[B];

		Print["i = ", i, "\tresB = \n", resB, "\n"];
		(*R = Map[(Join[{ { currA } }, #1])&, resB];*)


		R = Map[(Prepend[#1, currA])&, resB];
		Print["R = \n", R];

		(*res = Join[res, R];*)
		res = Join[res, R];

		
	];
	(*res*)
	Return[res];
	res

];
