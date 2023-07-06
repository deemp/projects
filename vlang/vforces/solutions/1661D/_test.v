module main

import os
import v.util.diff {find_working_diff_command, color_compare_files}

// https://github.com/vlang/v/blob/master/doc/docs.md#test-files
// https://github.com/vlang/v/blob/master/doc/docs.md#running-tests

fn single_test(test_number string) {
	wd := @FILE.split('/')#[..-1].join('/')
	test_data := "$wd/testdata/${test_number}."
	test_in := "${test_data}in"
	test_ans := "${test_data}ans"
	test_out := "${test_data}out"
	code := "$wd/sol.v"
	
	res := os.execute('v run $code < $test_in')
	if res.exit_code != 0 {
		println("RTE on test ${test_number}")
	} else {
		println("Test $test_number:\n\nYour answer:\n\n${res.output}")
		println("\nCorrect answer:\n")
		println(os.execute("cat $test_ans").output)
	}
}

fn test_many() {
	println("\nRunning tests...\n")

	for i in os.args[1..] {
		single_test(i)
	}
}