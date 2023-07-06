#!/usr/bin/env -S v run

import os

wd := os.getwd()
problem := os.args[1]
problem_dir := "$wd/solutions/${problem}"
tests := os.args[2..].map(it.str()).join(' ')
res := os.execute("v watch -s -c run $problem_dir/_test.v ${tests}")
println(res)