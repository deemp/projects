fn main() {
    lecture1::main()
}

pub mod lecture1 {
    // https://www.youtube.com/watch?v=XDv4I3_4Ubs&list=PL4_hYwCyhAvbeLzi699gqMUA4UaPkcdmJ
    use std::vec;

    // slide 50
    fn _50() {
        let idx: usize = 92;
        let y = 92_000_000i64;
        let hex_octal_bin: i64 = 0xffff_ffff + 0o777 + 0b1;
        let mut idx: usize = 0x1022022;
    }

    fn _51() {
        // bool - 1 byte in memory
        let mut x = true;
        x = false;
        // x = 1;
    }

    fn _52() {
        // "/" rounds to 0
        let (x, y) = (15, -15);
        let (a1, b1) = (x / -4, x % -4);
        let (a2, b2) = (y / 4, y % 4);
        let (a3, b3) = (x / 4, x % 4);
        println!("Division, mod:\n{a1} {b1} and {a2} {b2} and {a3} {b3}");

        println!(
            "AND: {a1}, NOT: {a2}, SHL: {a3}",
            a1 = 3 & 5,
            a2 = !3,
            a3 = 3 << 1
        );

        println!(
            "ABS: {a1}, count ones: {a2}, pow: {a3}",
            a1 = (-92i32).abs(),
            a2 = 0b001100u8.count_ones(),
            a3 = 2i32.pow(10)
        )
    }

    fn _53() {
        let x: u16 = 1;

        // let y: u32 = x;

        // explicit casting
        let y: u32 = x.into();

        // filling leading bits with zeros
        let z: u16 = y as u16;

        // cutting leading zeros
        let to_usize = 92u64 as usize;
        let from_usize = 92usize as u64;
    }

    fn _54() {
        // Casting isn't transitive
        // e as U1 as U2 != e as U2
    }

    fn _55() {
        let x = i32::max_value();
        let y = x + 1;
        println!("{y}");
    }

    fn _56() {
        let x = i32::MAX;

        let y = x.wrapping_add(1);
        assert_eq!(y, i32::MIN);

        let y = x.saturating_add(1);
        assert_eq!(y, i32::MAX);

        let (y, overflowed) = x.overflowing_add(1);
        assert!(overflowed);
        // assert_eq!(y, i32::MAX);

        match x.checked_add(1) {
            Some(y) => unreachable!(),
            None => println!("overflowed"),
        }
    }

    fn _57() {
        let y = 0.0f32;
        let x = 0.0;

        // point necessary for floats
        let z = 0.0f32;

        let not_a_number = std::f32::NAN;
        assert!(not_a_number != not_a_number);
        let inf = std::f32::INFINITY;

        println!("{a}", a = 8.5f32.ceil().sin().ceil().sqrt())
    }

    fn _58() {
        let to_be = true;
        let not_to_be = !to_be;
        let the_question = to_be || not_to_be;

        // && and || are lazy

        println!("{the_question}")
    }

    fn _59() {
        let pair: (f32, i32) = (0.0, 92);
        let (x, y) = pair;

        // note shadowing
        let x = pair.0;
        let y = pair.1;

        let void_result = println!("hello");
        assert_eq!(void_result, ());

        let trailing_comma = ("a", "b");
    }

    fn _61() {
        let x = 10;
        for i in 0..5 {
            if x == 10 {
                let x = 12;
                println!("{i}: {x}");
            }
        }
    }

    fn _62() {
        fn shadowing(num: i32) -> i32 {
            let vec = vec![0, 1, 2, 3];
            let vec = vec![4, 5, 6, 7];
            vec[0]
        }
        println!("{a}", a = shadowing(5));
    }

    fn _63() {
        let x = ();
        let y = {};
        assert!(x == y);

        // traling comma necessary
        // (42) == 42
        let x = (42,);

        println!("{a}", a = x.0)
    }

    fn _65() {
        // tuple is stored in memory continuously
        let t = (92, 93);
        println!("{:?}", &t as *const (i32, i32));
        println!("{:?}, {:?}", &t.0 as *const i32, &t.1 as *const i32);

        println!("{:?}, {:?}", &t.0 as *const i32, &t.1 as *const i32);
    }

    fn _67() {
        // type and number of elements
        let xs: [u8; 3] = [1, 2, 3];
        assert_eq!(xs[0], 1);
        assert_eq!(xs.len(), 3);

        // let mut buf = [0u8, 1024];
        let mut buf = [0u8, 124];
    }

    fn _68() {
        let mut x = 92;

        // reference to a mutable i32
        let r: &mut i32 = &mut 92;

        let r: &mut i32 = &mut x;
        *r += 1;

        println!("{r}")
    }

    fn _69() {
        // to be used with unsafe

        // constant pointer to a constant i32
        let x: *const i32 = std::ptr::null();
        // mutable pointer to a constant i32
        let mut y: *const i32 = std::ptr::null();
        //
        let z: *mut i32 = std::ptr::null_mut();
        let mut t: *mut i32 = std::ptr::null_mut();
    }

    fn _71() {
        // pointer to data on heap
        let x: Box<i32> = Box::new(92);
    }

    fn _72() {
        fn func1() {}
        fn func2() -> () {}
        fn func3() -> i32 {
            0
        }
        fn func4(x: u32) -> u32 {
            return x;
        }
        fn func5(x: u32, mut y: u64) -> u64 {
            y = x as u64 + 10;
            return y;
        }
        fn func6(x: u32, mut y: u64) -> u32 {
            x + 10
        }
    }

    fn _73() {
        // expressions and statements
        let y = 42;
        let x = if y < 42 { 345 } else { y + 534 };
    }

    pub mod _74 {

        struct Example {
            oper_count: usize,
            data: Vec<i32>,
        }

        // no guarantees about default memory representation of structures
        // A and B can be different in memory
        struct A {
            x: Example,
        }

        struct B {
            y: Example,
        }
    }

    fn _75_76_77() {
        struct Example {
            oper_count: usize,
            data: Vec<i32>,
        }

        impl Example {
            // Associated

            pub fn new() -> Self {
                Self {
                    oper_count: 0,
                    data: Vec::new(),
                }
            }

            // Non-associated because it takes a reference to mutable Example
            pub fn push(&mut self, x: i32) {
                self.oper_count += 1;
                self.data.push(x)
            }
        }

        // another impl block
        impl Example {
            // takes a read-only reference to self
            pub fn oper_count(&self) -> usize {
                self.oper_count
            }

            // TODO does the object move to self?
            pub fn eat_self(self) {
                println!("later on lecture :)")
            }
        }

        let mut x = Example {
            oper_count: 0,
            data: Vec::new(),
        };

        let y = Example::new();
        x.push(10);
        assert_eq!(x.oper_count(), 1);
    }

    pub mod _78_79_80 {
        pub struct Example<T> {
            oper_count: usize,
            data: Vec<T>,
        }

        impl<T> Example<T> {
            pub fn new() -> Self {
                Self {
                    oper_count: 0,
                    data: Vec::new(),
                }
            }

            pub fn push(&mut self, x: T) {
                self.oper_count += 1;
                self.data.push(x);
            }

            // takes a read-only reference to self
            pub fn oper_count(&self) -> usize {
                self.oper_count
            }

            // TODO does the object move to self?
            pub fn eat_self(self) {
                println!("later on lecture :)")
            }
        }

        pub fn main() {
            // Generics

            // turbofish ::<>
            let mut x = Example::<i32> {
                oper_count: 0,
                data: Vec::new(),
            };

            let y = Example::<i32>::new();
            let z: Example<i32> = Example {
                oper_count: 0,
                data: Vec::new(),
            };

            let z: Example<i32> = z;

            x.push(10);
            assert_eq!(x.oper_count(), 1);
        }
    }

    fn _81() {
        let mut x = 2;
        if x == 2 {
            x += 2;
        }
        while x > 0 {
            x -= 1;
            println!("{x}")
        }
    }

    fn _82() {
        println!("I'm infinite");
        let mut x = 2;
        loop {
            x += 1;
            if x == 10 {
                println!("I lied...");
                break;
            }
        }
    }

    fn _83() {
        let mut counter = 0;

        // can return a value from loop
        let result = loop {
            counter += 1;
            if counter == 10 {
                break counter * 2;
            }
        };
        assert_eq!(result, 20)

        // Default break is `break ()`
    }

    fn _84() {
        'outer: loop {
            println!("Entered the outer loop");
            'inner: for _ in 0..10 {
                println!("Entered the inner loop");

                // This breaks the inner loop
                // break;

                // This breaks the outer loop
                break 'outer;
            }
            println!("This point will never be reached");
        }
        println!("Exited the outer loop");
    }

    fn _85() {
        println!("[0;10)");

        for i in 0..10 {
            println!("{i}")
        }

        println!("\n[0;10]");

        for i in 0..=10 {
            println!("{i}")
        }

        println!("\n[1;4]");
        for i in [1, 2, 3, 4] {
            println!("{i}")
        }
    }

    fn _86() {
        let vec = vec![1, 2, 3, 4];

        println!("Iterate a reference to a vector");

        for i in &vec {
            println!("{i}")
        }

        println!("\nIterate a vector");

        for i in vec {
            println!("{i}")
        }
    }

    pub mod _87 {
        use crate::lecture1::_78_79_80::Example;

        pub enum MyEnum {
            First,
            Second,
            Third,
        }

        pub enum OneMoreEnum<T> {
            Eins(i32),
            Zwei(u64, Example<T>),
        }

        pub fn main() {
            let x = MyEnum::First;
            let y: MyEnum = MyEnum::First;
            let z = OneMoreEnum::Zwei(42, Example::<usize>::new());
        }
    }

    fn _88() {
        // Maybe t
        enum Option<T> {
            Some(T),
            None,
        }

        // Either t e
        enum Result<T, E> {
            Ok(T),
            Err(E),
        }
    }

    fn _89() {
        use crate::lecture1::_87::MyEnum;

        let x = MyEnum::First;
        match x {
            MyEnum::First => println!("First"),
            MyEnum::Second => {
                for i in 0..5 {
                    println!("{i}");
                }
                println!("second");
            }
            _ => println!("Matched something!"),
        }
    }

    fn _90() {
        // _ - wildcard in `match`

        // type inference
        let mut vec: Vec<_> = (0..10).collect();
        vec.push(42u64);

        // making variable unused
        let _x = 10;
    }

    fn _91() {
        use crate::lecture1::_87::{MyEnum, OneMoreEnum};
        // matching multiple objects

        let x = OneMoreEnum::<i32>::Eins(2);
        let y = MyEnum::First;
        match (x, y) {
            (OneMoreEnum::Eins(x), MyEnum::First) => {
                println!("Hello")
            }
            (OneMoreEnum::Zwei(a, _), _) => println!("{a}"),
            _ => println!("oooof!"),
        }
    }

    fn _92() {
        let number = 13;
        match number {
            1 => println!("One!"),
            // matching multiple patterns
            2 | 3 | 5 | 7 | 11 => println!("This is a prime"),
            // matching a range
            13..=19 => println!("A teen"),
            _ => println!("Ain't special"),
        }
    }

    fn _93() {
        let pair = (2, -2);
        // {:?} calls Debug instead of Display trait implementation - https://stackoverflow.com/a/38157410
        println!("Tell me about {pair:?}");

        // guards
        match pair {
            (x, y) if x == y => println!("These are twins"),
            (x, y) if x + y == 0 => println!("Antimatter, kaboom"),
            (x, _) if x % 2 == 1 => println!("The first one is odd"),
            _ => println!("No correlation"),
        }
    }

    fn _94() {
        let boolean = 13;

        fn foo() -> bool {
            false
        }

        let x = match boolean {
            13 if foo() => 0,
            13 => 1,
            _ => 2,
        };
    }

    fn _95() {
        let triple = (0, -2, 3);

        // ignore rest of the tuple
        let (x, ..) = triple;
        println!("{x}")
    }

    fn _96_97() {
        struct Foo {
            x: (u32, u32),
            y: u32,
        }

        let foo = Foo { x: (1, 2), y: 3 };

        match foo {
            Foo { x: (1, b), y } => {
                println!("First of x is 1, b = {b}, y = {y}")
            }
            Foo { y, .. } => {
                println!("y = {y}, we don't care about x")
            }
            // doesn't mention x
            // Foo { y } => println!("y = {y}"),
        }
    }

    fn _98() {
        // pattern synonym

        match 7 {
            n @ 1..=12 => println!("It's {n}, actually"),
            n => println!("Whatever ({n})"),
        }
    }

    fn _99() {
        // pattern synonym for slices

        let s = [1, 2, 3, 4];
        // or s.as_slice()
        let mut t = &s[..];
        loop {
            match t {
                [head, tail @ ..] => {
                    println!("{head}");
                    t = &tail
                }
                _ => break,
            }
        }
    }

    fn _100() {
        enum Test {
            A(i32),
            B,
        }

        impl Test {
            pub fn unwrap(self) -> i32 {
                match self {
                    Self::A(x) => x,
                    Self::B => panic!("error!"),
                }
            }
        }

        let t = Test::A(3);

        println!("{a}", a = t.unwrap())
    }

    fn _101() {
        let optional = Some(7);
        match optional {
            Some(i) => {
                println!("It's Some({i})")
            }
            _ => {}
        }
    }

    fn _102() {
        // == _101

        // if let pattern matches a single option

        let optional = Some(7);
        if let Some(x) = optional {
            println!("{optional:?} contains {x}")
        }
    }

    fn _103() {
        let mut optional = Some(0);

        while let Some(i) = optional {
            if i > 9 {
                println!("Greater than 9, quit!");
                optional = None;
            } else {
                println!("`i` is `{i}`. Try again.");
                optional = Some(i + 1);
            }
        }
    }

    fn _105() {
        enum Test {
            First(bool),
            Second,
            Third,
            Fourth,
        }
        assert_eq!(std::mem::size_of::<Test>(), 1);
        assert_eq!(std::mem::size_of::<Option<Box<i32>>>(), 8)
    }

    fn _106() {
        enum Test1 {
            First(bool, bool),
            Field0,
            // more fields ...
            Field253,
        }

        enum Test2 {
            First(bool, bool),
            Field0,
            // more fields
            Field254,
        }
        // size of an enum should be size(largest element) + size(discriminant)
        // discriminant identifies options and takes 8 bytes
        // discriminant can be embedded into existing bytes to optimize the enum size ("niche filling")
        // https://togglebit.io/posts/enums/
        assert_eq!(std::mem::size_of::<Test1>(), 2);
        assert_eq!(std::mem::size_of::<Test2>(), 2);
    }

    fn _107_108() {
        // Vector

        pub struct Vec<T> {
            ptr: *mut T,
            length: usize,
            capacity: usize,
        }

        use std::mem::size_of;

        // why 3?
        // ptr, length, and capacity are all of the size `usize`
        assert_eq!(size_of::<Vec<i32>>(), size_of::<usize>() * 3);

        // vector with the same element (42) repeated 42 times
        let mut xs = vec![42; 42];
        assert_eq!(xs.len(), 42);

        let mut xs = vec![1, 2, 3];
        xs.push(4);
        assert_eq!(xs.len(), 4)
    }

    fn _109() {
        // Slices

        // a is an array
        let a = [1, 2, 3, 4, 5];

        // https://doc.rust-lang.org/stable/reference/types/slice.html

        // borrows data it points to
        let slice1 = &a[1..4];

        // indices are usize -> uses indices [0,1]
        let slice2 = &slice1[..2];
        assert_eq!(slice1, &[2, 3, 4]);
        assert_eq!(slice2, &[2, 3]);
    }

    fn _110() {
        let x = 42;
        if x == 42 {
            panic!("The answer")
        }

        // Other panic macros:
        // unimplemented, unreachable, todo, assert, assert_eq
    }

    fn _111() {
        let x = 42;

        // zero padding
        println!("{x:04}");

        // pretty debug
        let y = (100, 200);
        println!("{:#?}", y);
    }

    fn _112() {
        fn func() -> i32 {
            // allows code to typecheck
            unimplemented!("not yet ready")
        }
    }

    fn _extra(){
        // https://saghm.github.io/five-rust-things/
    }

    pub fn main() {
        _111()
    }
}
