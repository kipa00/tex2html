import sys, os

problem_name = sys.argv[1]
if not os.system("cp ../statements/%s/problem.tex temp.tex" % problem_name):
	os.system("nano temp.tex")
	os.system("./main < temp.tex")
