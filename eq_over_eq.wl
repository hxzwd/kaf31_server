

Print["Load functions from lib_funcs.wl...\n"];
Get["lib_funcs.wl"];

Print["Load equations data from eqs_data.wl...\n"];
Get["eqs_data.wl"];


$var = Null;



fInitDebug[] := Module[ { },


	(*
	eqsData = <| |>;
	eqsData = EQSDATA;
	*)

	Print["init debug mode\n"];
	Print["current context is ", Context[], "\n"];

	$var = <| |>;
	$var["debug"] = <| |>;
	$var["f add"] = Function[ { key, obj }, $var["debug"][key] = obj ];
	$var["add"] = Functon[ { key, obj }, $var[key] = obj ];
	$var["join"] = Function[{}, $var = Join[$var, $var["debug"]]];
	(*$var["eqs data"] = <| |>;*)
	$var["eqs data"] = <| |>;

];

fDebugAddObj[key_, obj_] := Module[ {},

	$var["debug"][key] = obj;
];

fDebugWriteEq[ eqName_, eqDesc_ ] := Module[ { },

	$var["eqs data"][eqName] = eqDesc;

];

fDebugReadEq[ eqName_ ] := Module[ { res },

	res = $var["eqs data"][eqName];
	res

];




fGetTargetEq[] := Module[ { res, params, targetEq, poleOrder, eqDesc },

	res = <| |>;

	eqDesc = fDebugReadEq["KdVBEq"];

	params = eqDesc[[1]];
	solveparams = eqDesc[[2]];
	poleOrder = eqDesc[[3]];
	targetEq = eqDesc[[4]];


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




drun[] := Module[ { NN, tmp, eqInfo, eq, p, tmp0, finalEq, paramSys, tmp1, tmp2, cclist, pres },


	(*Get["drun.wl"];*)

	NN = 4;

	tmp = fGenSubs[NN];

	eqInfo = fGetTargetEq[];
	eq = eqInfo["eq"];
	p = eqInfo["p"];
	solveparams = eqInfo["solveparams"];


	tmp0 = fSubYSeries[ eq, p ];

	finalEq = tmp0["final eq"];

	tmp1 = fGenSystemForParams[finalEq, p];
	paramSys = tmp1["coeff list"];

	tmp2 = fGetSeriesCoeffs[paramSys, p];

	$var["f add"]["eqs", fMakeEqs[paramSys]];


	$var["f add"]["final eq", finalEq];
	$var["f add"]["tmp0", tmp0];
	$var["f add"]["tmp1", tmp1];
	$var["f add"]["tmp2", tmp2];
	$var["f add"]["eq", eq];
	$var["f add"]["eqInfo", eqInfo];
	$var["f add"]["tmp", tmp];
	$var["f add"]["p", p];
	$var["f add"]["solveparams", solveparams];


	cclist = Values[tmp2];
	solstree = { };

	solstree = ALG[cclist];

	$var["f add"]["solstree", solstree];

	(*pres = GETPARAMS[paramSys, solstree, p, { sigma, b, C1 }];*)


	(*
	solveparams = { sigma, b, C1 };
	solveparams = { alpha, gamma };
	*)


	pres = { };
	pres = GETPARAMS[paramSys, solstree, p, solveparams];
	$var["f add"]["pres", pres];
	

	$var["join"][];

	tmp2

];

fInitDebug[];
$var["eqs data"] = EQSDATA;
dadd = $var["add"];

q = drun[];
coeffs = $var["tmp1"]["coeff list"];
feq0 = $var["tmp0"]["eq after sub"];
feq1 = $var["tmp0"]["final eq"];

tmp = $var["tmp"];
tmp0 = $var["tmp0"];
tmp1 = $var["tmp1"];
tmp2 = $var["tmp2"];

eq = $var["eq"];
p = $var["p"];
eqinfo = $var["eqInfo"];
solveparams = $var["solveparams"];

solstree = $var["solstree"];
pres = $var["pres"];

finalParams = pres["final params"];

Print["\n\nFINAL PARAMS:\n\n", finalParams];

