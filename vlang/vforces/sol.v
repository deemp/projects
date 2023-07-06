import os {input}
import arrays {sum}
import math {min}

// custom IO
fn read_strings() ([]string) {return os.input('').split(' ')}
fn read_longs() ([]i64) {return read_strings().map(it.i64())}
fn read_ints() ([]int) {return read_strings().map(it.int())}
fn read_floats() ([]f64) {return read_strings().map(it.f64())}

struct Readers {
	mut :
		longs []i64 = []i64{}
		ints []int = []int{}
		floats []f64 = []f64 {}
		strings []string = []string{}
}

// solution


// constants

const (
	max_n = 200_009
)

// "global" variables

struct Context {
}

fn main() {
	mut r := Readers {}
	mut c := Context {}

}