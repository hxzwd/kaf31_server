
:Begin:
:Function:	wstpTest
:Pattern:	wstpTest[i_Integer, j_Integer]
:Arguments:	{ i, j }
:ArgumentTypes:	{ Integer, Integer }
:ReturnType:	Integer
:End:

:Evaluate:	wstpTest::usage = "wstpTest[x, y] -> return x + y"

int wstpTest(int i, int j)
{
	return i + j;
}

int main(int argc, char *argv[])
{
	return WSMain(argc, argv);
}



