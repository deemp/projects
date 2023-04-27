module main

import os {input}
import arrays {sum}
import math {min}

// custom IO
fn read_strings() ([]string) {return os.input('').split(' ')}
fn read_ints() ([]i64) {return read_strings().map(it.i64())}
fn read_floats() ([]f64) {return read_strings().map(it.f64())}


// main script

fn main() {
	mut ints := []i64{}
	// mut floats := []f64{}
	// mut strings := []string{}

	ints = read_ints()
	n, k := ints[0], ints[1]
	mut b := read_ints()
	mut d := []i64{len : int(n), init: 0}
	mut s := i64(0)
	mut total := i64(0)
	for i := n-1; i > -1; i-- {
		b[i] -= total
		mut p := i64(0)
		if b[i] > 0 {
			p = min<i64>(k, i+1)
			d[i] = (b[i] + p - 1) / p
		}
		s += d[i] - if i + k < n {d[i + k]} else {0}
		total += d[i] * p - s
	}

	sm := sum<i64>(d) or { -1}

	println('$sm')
}