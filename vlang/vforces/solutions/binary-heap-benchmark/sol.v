import time

// constants

const (
	max_n = 1_0_000_000
)

// "global" variables

struct Context {
	mut :
		h []int = []int{len: max_n, init: it}
}


// don't check bounds - see https://github.com/vlang/v/blob/master/doc/docs.md#performance-tuning
[direct_array_access]
// control a function's context
fn (mut c Context) push_down(pos int, n int) {
	mut p := pos
	for 2 * p + 1 < n {
		mut j := 2 * p + 1
		if j + 1 < n && c.h[j+1] > c.h[j] {
			j += 1
		}
		if c.h[p] >= c.h[j] {
			break
		}
		c.h[p], c.h[j] = c.h[j], c.h[p]
		p = j
	}
}

fn run_test() time.Duration{
	start := time.now()
	
	mut c := Context {}
	
	for i := max_n/2; i >= 0; i-- {
		c.push_down(i, max_n)
	}
	for i := max_n-1; i >= 0; i-- {
		c.h[0], c.h[i] = c.h[i], c.h[0]
		c.push_down(0, i)
	}
	for i in 0..max_n {
		assert c.h[i] == i
	}

	finish := time.now()
	return finish - start
}

fn main() {
	mut average := 0.0
	n := 10
	for _ in 0..n {
		average += run_test().milliseconds()
	}
	average /= n
	print("\nDone on average in ${average:.2} ms\n")
}