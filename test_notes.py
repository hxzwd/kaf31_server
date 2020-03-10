#!/usr/local/bin/ipython
#-*- coding: utf-8 -*-


import os
import sys

from IPython import embed

from wolframclient.evaluation import WolframLanguageSession
from wolframclient.language import wl, wlexpr


class c_wl:

	session = None

	def __init__(self):
		kernel = "/usr/local/Wolfram/Mathematica/11.2/Executables/WolframKernel"
		self.session = WolframLanguageSession(kernel)


	def do(self, expr):
		return self.session.evaluate(wlexpr(expr))


def main():
	wl_str = "Map[&(#1^2), Range[1, 13, 2]]"

	wl_session = WolframLanguageSession()
	test_wl_expr = wlexpr(wl_str)
	test_res = wl_session.evaluate(test_wl_expr)

	return test_res

wl_str = "Map[&(#1^2), Range[1, 13, 2]]"

wl_obj = c_wl()
test_res = wl_obj.do(wl_str)


embed()


