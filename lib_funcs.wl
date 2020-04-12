


fGetTargetEqOld[] := Module[ { res, params, targetEq, poleOrder },

	res = <| |>;


	(*ksEq*)
	(*{ sigma, b, C1 }*)
	params = { sigma, C0, C1 };
	solveparams = { sigma, b, C1 };
	poleOrder = 3;
	targetEq =
		D[y[z], {z, 3}] + sigma*D[y[z], {z, 2}] +
		D[y[z], {z, 1}] + y[z]^2/2 - C0*y[z] + C1;


	(*REq*)
	(*{ alpha, gamma }*)
	params = { alpha, beta, gamma };
	solveparams = { alpha, gamma };
	poleOrder = 2;
	targetEq =
		D[y[z], {z, 1}]^2 - 4*y[z]^3 -
		alpha*y[z]^2 - beta*y[z] - gamma;


	(*KdVBEq*)
	(*{ alpha, b }*)
	params = { alpha, C0, C1 };
	solveparams = { alpha, b };
	poleOrder = 2;
	targetEq =
		D[y[z], {z, 2}] + 3*y[z]^2 -
		alpha*D[y[z], {z, 1}] - C0*y[z] + C1;



	res["target equation"] = targetEq;
	res["pole order"] = poleOrder;
	res["list of params"] = params;
	res["restrict via params"] = solveparams;

	res["eq"] = res["target equation"];
	res["p"] = res["pole order"];
	res["params"] = res["list of params"];
	res["solveparams"] = res["restrict via params"];

	res

];


fGetSimpEq[] := Module[ { simpEq },

	simpEq = D[Y[z], { z, 1 }] + Y[z]^2 - b;

	simpEq

];

fGetSimpEqFunc[] := Module[ { simpEq, simpEqFunc },

	simpEq = fGetSimpEq[];

	simpEqFunc = DSolve[simpEq == 0, Y[z], z][[1]]/.C[1]->Csimp;

	simpEqFunc

];

fGetEq[eq_, subs_] := Module[ { res },

	res = D[eq, { z, 1 }]/.subs;

	res

];

fGenSubs[NN_] := Module[ { res },

	res = { D[Y[z], {z, 1}] -> -Y[z]^2 + b };

	For[i = 1, i <= NN, i++,

		res = Append[res, D[Y[z], {z, i + 1}] ->
				fGetEq
				[
					res[[i]][[2]],
					res
				]
			]

	];

	res
];


fGenSeriesCoeffs[p_] := Module[ { res },

	res = Map[(StringTemplate["A``"][#1])&, Range[0, p]];
	res = "{ " <> StringRiffle[res, ", "] <> " }";
	res = ToExpression[res];

	res

];

fGetYSeries[p_] := Module[ { solser, coefflist },

	coefflist = fGenSeriesCoeffs[p];

	solser = Map[(coefflist[[#1 + 1]] * Y[z]^#1)&, Range[1, p]];
	solser = Total[solser] + coefflist[[1]];

	solser
	

];

fSubYSeries[eq_, p_] := Module[ { res, subExp, ySubs, eqAfterSub, finalEq },

	subExp = Function
	[ { x },
		fGetYSeries[p]/.{ z -> x }
	];
	
	ySubs = fGenSubs[p];
	res = <| |>;

	res["Y subs list"] = ySubs;
	res["y sub"] = subExp;

	eqAfterSub = eq/.{ y -> subExp };
	res["eq after sub"] = Expand[Simplify[eqAfterSub]];


	(*finalEq = eqAfterSub/.Reverse[ySubs];*)
	finalEq = eqAfterSub;
	ySubs = Reverse[ySubs];


	For[i = 1, i <= Length[ySubs], i++,
		finalEq = finalEq/.ySubs[[i]];
		finalEq = Expand[Simplify[finalEq]];
	]

	finalEq = Simplify[finalEq];
	res["final eq"] = finalEq;

	res

];


fGetPolyCoeffs[expLimit__] := Module[ { res },

	res = Map[ (Y[z]^#1)&, Range[1, expLimit] ];

	res
];

fGenSystemForParams[finalEq_, p_] := Module[ { res, polyCoeffs, maxPolyExp, coefflist, freecoeff },

	maxPolyExp = Exponent[finalEq, Y[z]];
	polyCoeffs = Reverse[fGetPolyCoeffs[maxPolyExp]];

	res = <| |>;


	coefflist = Map[(Coefficient[finalEq, #1])&, polyCoeffs];
	freecoeff = finalEq/.{ Y[z] -> 0 };
	freecoeff = Simplify[freecoeff];
	coefflist = Join[ coefflist, { freecoeff } ];


	res["coeff list"] = coefflist;
	res["poly coeffs"] = polyCoeffs;
	res["max poly exp"] = maxPolyExp;

	res

];

FVL[val_] := Module[ { res },

	res = Map[(#1[[-1]])&, val];

	res

];

FCL[val_] := Module[ { res },
	res = val[[1]][[1]];
	res
];

AT[obj_, i_] := Module[ { res },
	res = obj[[i]];
	res
];

ATL[obj_, il_] := Module[ { res },
	res = obj;
	For[i = 1, i <= Length[il], i++,
		res = obj[[il[[i]]]];
	];
	res
];

FEXP0[sol_, sub_] := Module[ { res, cc },

	cc = sol[[1]][[1]];

	res = Map[(cc -> FVL[sol]/.#1)&, sub];
	res

];

(*
FEXP2[sl_, t_] := Module[ { res },

	res = t;

	For[i = 1, i <= Length[sl], i++,
		res = FEXP0[t, sl[[i]]];

];
*)

FEXP1[ll_] := Module[ { res, hcoeff, cc, subs },

	res = <| |>;
	hcoeff = FCL[ll[[1]]];
	res[hcoeff] = ll[[1]];

	For[i = 1, i < Length[ll], i++,
		cc = FCL[ll[[i + 1]]];
		subs = res[[i]];
		res[cc] = FEXP0[ll[[i + 1]], subs];
	];

	res

];

FSA[eqs_, index_] := Module[ { res, coeff, eq, csol, jsol },

	res = <| |>;

	eq = eqs[[index]];
	coeff = ToExpression[StringTemplate["A``"][p - index + 1]];
	csol = Solve[eq, coeff];
	jsol = Join@@csol;

	(*jsol = Cases[jsol, Except[coeff -> 0]];*)
	
	res["coeff value"] = csol;
	res["coeff"] = coeff;
	res["new eqs"] = eqs;
	res["final value"] = csol;
	res["joined values"] = jsol;


	res
	
];

fMakeEqs[sys_] := Module[ { eqs },

	eqs = Map[(#1 == 0)&, sys];
	eqs
];


fGetSeriesCoeffs[coefflist_, p_] := Module[ { res, targets, eqs, fsaRes, values, fexp1res, hcoeff, hctmp },

	coefflist = coefflist[[ 1;;p + 1 ]];
	eqs = fMakeEqs[coefflist];
	targets = fGenSeriesCoeffs[p];
	targets = Reverse[targets];
	
	res = <| |>;
	fsaRes = <| |>;

	For[i = 1, i <= p + 1, i++,
		fsaRes = FSA[eqs, i];
		values = fsaRes["joined values"];
	
		res[targets[[i]]] = values;
	];

	fexp1res = FEXP1[res];
	
	hcoeff = Keys[res][[1]];
	hctmp = { };
	res[hcoeff] = Cases[res[hcoeff], Except[hcoeff -> 0]];
	

	res


];




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



GETPARAMS[SYS_, SOLSTREE_, P_, PARAMS_] := Module[ { res, targetSys, subsys, eqsys, finparams },

	res = <| |>;

	targetSys = SYS[[P + 2;;]];

	(*subsys = targetSys/.SOLSTREE;*)
	(*subsys = targetSys;*)

	subsys = Simplify[targetSys/.SOLSTREE];

	eqsys = Map[(Map[Function[{x}, x == 0], #1])&, subsys];

	(*eqsys = Map[Function[ { psys }, Map[(#1 == 0)&, Simplify[psys/.SOLSTREE]]], subsys];*)

	finparams = Map[(Solve[#1, PARAMS])&, eqsys];

	res["final params"] = finparams;
	res["target coeffs"] = targetSys;
	res["eqs systems"] = eqsys;

	Return[res];

	res

];


