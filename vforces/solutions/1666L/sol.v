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

// constants

const (
	max_n = 200_009
)

// "global" variables

struct Context {
	mut :
		p []int = []int{len: max_n, init: 0}
		q []int = []int{len: max_n, init: 0}
		g [][]int = [][]int{len: max_n, init: []int {}}
		n int
		m int
		s int
}

// control a function's context

fn (mut c Context) dfs(x int, y int, z int) {
	if x == c.s && y != 0 {return}
	
	if c.p[x] != 0 && c.q[x] == z {return}
	
	if c.p[x] != 0 {
		println('Possible')
		mut v := [x]
		w := x
		for i := x; i != c.s; {
			i = c.p[i]
			v << i
		}
		println('${v.len}')
		for i in v.reverse() { print('$i ') }
		println('')
		
		v = [w, y]
		for i := y; i != c.s; {
			i = c.p[i]
			v << i
		}
		println('${v.len}')
		for i in v.reverse() { print('$i ') }
		println('')
		
		exit(0)
	}

	c.p[x] = y
	c.q[x] = z
	
	if z == 0 {
		for i in c.g[x] {
			c.dfs(i, x, i)
		}
	} else {
		for i in c.g[x] {
			c.dfs(i, x, z)
		}
	}
}

fn main() {
	mut r := Readers {}
	mut c := Context {}
	
	r.ints = read_ints()
	c.n, c.m, c.s = r.ints[0], r.ints[1], r.ints[2]

	for _ in 0..c.m {
		r.ints = read_ints()
		i, j := r.ints[0], r.ints[1]
		c.g[i] << j
	}

	c.dfs(c.s,0,0)

	println('Impossible')
}