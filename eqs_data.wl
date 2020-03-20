

EQSDATA =
<|
	
	"ksEq" ->
	{
		{ sigma, C0, C1 },
		{ sigma, b, C1 },
		3,
		D[y[z], {z, 3}] + sigma*D[y[z], {z, 2}] +
		D[y[z], {z, 1}] + y[z]^2/2 - C0*y[z] + C1
	},

	"REq" ->
	{
		{ alpha, beta, gamma },
		{ alpha, gamma },
		2,
		D[y[z], {z, 1}]^2 - 4*y[z]^3 -
		alpha*y[z]^2 - beta*y[z] - gamma
	},

	"KdVBEq" ->
	{
		{ alpha, C0, C1 },
		{ alpha, b },
		2,
		D[y[z], {z, 2}] + 3*y[z]^2 -
		alpha*D[y[z], {z, 1}] - C0*y[z] + C1
	},

	"nsEq" ->
	{
		{ alpha, beta },
		{ alpha, b },
		1,
		D[y[z], {z, 2}] + alpha*y[z] + beta*y[z]^3
	},


	"ee5Eq" ->
	{
		{ epsilon, gamma, delta, xi, beta, alpha, C0, C1 },
		{ b, xi, gamma },
		5,	
		epsilon*D[y[z], {z, 5}] + gamma*D[y[z], {z, 4}] +
		delta*D[y[z], {z, 3}] + xi*D[y[z], {z, 2}] +
		beta*D[y[z], {z, 1}] + alpha*y[z]^2 - C0*y[z] + C1
	}

|>;



