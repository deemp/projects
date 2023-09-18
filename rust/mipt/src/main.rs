fn main() {
    // lecture1::main()
    // lecture2::main()
    lecture3::main()
}

pub mod lecture1 {
    // https://www.youtube.com/watch?v=XDv4I3_4Ubs&list=PL4_hYwCyhAvbeLzi699gqMUA4UaPkcdmJ
    use std::vec;

    // slide 50
    fn _50() {
        let _idx: usize = 92;
        let _y = 92_000_000i64;
        let _hex_octal_bin: i64 = 0xffff_ffff + 0o777 + 0b1;
        let mut _idx: usize = 0x1022022;
    }

    fn _51() {
        // bool - 1 byte in memory
        let mut _x = true;
        _x = false;
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
        let _z: u16 = y as u16;

        // cutting leading zeros
        let _to_usize = 92u64 as usize;
        let _from_usize = 92usize as u64;
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

        let (_y, overflowed) = x.overflowing_add(1);
        assert!(overflowed);
        // assert_eq!(y, i32::MAX);

        match x.checked_add(1) {
            Some(_y) => unreachable!(),
            None => println!("overflowed"),
        }
    }

    fn _57() {
        let _y = 0.0f32;
        let _x = 0.0;

        // point necessary for floats
        let _z = 0.0f32;

        let not_a_number = std::f32::NAN;
        assert!(not_a_number != not_a_number);
        let _inf = std::f32::INFINITY;

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
        let (_x, _y) = pair;

        // note shadowing
        let _x = pair.0;
        let _y = pair.1;

        let void_result = println!("hello");
        assert_eq!(void_result, ());

        let _trailing_comma = ("a", "b");
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
        fn shadowing(_num: i32) -> i32 {
            let _vec = vec![0, 1, 2, 3];
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
        let mut _buf = [0u8, 124];
    }

    fn _68() {
        let mut x = 92;

        // reference to a mutable i32
        let _r: &mut i32 = &mut 92;

        let r: &mut i32 = &mut x;
        *r += 1;

        println!("{r}")
    }

    fn _69() {
        // to be used with unsafe

        // constant pointer to a constant i32
        let _x: *const i32 = std::ptr::null();
        // mutable pointer to a constant i32
        let mut _y: *const i32 = std::ptr::null();
        //
        let _z: *mut i32 = std::ptr::null_mut();
        let mut _t: *mut i32 = std::ptr::null_mut();
    }

    fn _71() {
        // pointer to data on heap
        let _x: Box<i32> = Box::new(92);
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
        fn func6(x: u32, mut _y: u64) -> u32 {
            x + 10
        }
    }

    fn _73() {
        // expressions and statements
        let y = 42;
        let _x = if y < 42 { 345 } else { y + 534 };
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

        let _y = Example::new();
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

            let _y = Example::<i32>::new();
            let z: Example<i32> = Example {
                oper_count: 0,
                data: Vec::new(),
            };

            let _z: Example<i32> = z;

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
            '_inner: for _ in 0..10 {
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
            let _x = MyEnum::First;
            let _y: MyEnum = MyEnum::First;
            let _z = OneMoreEnum::Zwei(42, Example::<usize>::new());
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
            (OneMoreEnum::Eins(_x), MyEnum::First) => {
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

        let _x = match boolean {
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

    fn _extra() {
        // https://saghm.github.io/five-rust-things/
    }

    fn _116() {
        fn problem() {
            let mut v = vec![1, 2, 3];
            // x is a reference to v
            // hence, v becomes immutable
            let x = &v[0];

            // cannot borrow `v` as mutable because it is also borrowed as immutable
            // v.push(4);

            // if comment this out and not use x,
            // x will be dead code => removed => v will continue being mutable
            println!("{x}")
        }

        fn solution() {
            let mut v = vec![1, 2, 3];
            v.push(4);
            let x = &v[0];
            println!("{x}")
        }
    }

    fn _118() {
        fn problem() {
            // rust has move-semantics by default
            // vector is moved here
            fn sum(v: Vec<i32>) -> i32 {
                let mut result = 0;
                for i in v {
                    result += i
                }
                result
            }

            fn main() {
                let mut v = vec![1, 2, 3];
                println!("first sum: {a}", a = sum(v));

                // borrow of moved value: `v`
                // move occurs because `v` has type `std::vec::Vec<i32>`, which does not implement the `Copy` trait
                // v.push(4);

                // use of moved value: `v`
                // println!("second sum: {a}", a = sum(v))
            }
        }

        pub fn solution() {
            // rust has move-semantics by default
            // vector is moved here
            fn sum(v: &Vec<i32>) -> i32 {
                let mut result = 0;
                for i in v {
                    result += i
                }
                result
            }

            fn main() {
                let mut v = vec![1, 2, 3];
                println!("first sum: {a}", a = sum(&v));

                // borrow of moved value: `v`
                // move occurs because `v` has type `std::vec::Vec<i32>`, which does not implement the `Copy` trait
                v.push(4);

                // use of moved value: `v`
                println!("second sum: {a}", a = sum(&v))
            }

            main()
        }

        fn extra() {
            // Copy is a marker trait - copy this structure byte by byte
            // There's no default Copy and Clone for struct
            // because Rust doesn't know how you use them

            // e.g., a struct contains an identifier that should be unique among structs,
            // so copying the struct is unsafe
        }

        solution()
    }

    fn _120() {
        // - Each value in Rust has a variable that’s called it’s owner.
        // - There can be only one owner at a time.
        // - When the owner goes out of scope, the value will be dropped.
    }

    fn _121() {
        let s = vec![1, 4, 8, 8];
        let u = s;
        println!("{:?}", u);

        // borrow of moved value: `s`
        // value borrowed here after move
        // println!("{:?}", s);
    }

    fn _123() {
        // primitive types have Copy implemented

        fn om_nom_nom(n: u32) {
            println!("{n} is a very nice number")
        }

        let n: u32 = 110;
        let m = n;
        om_nom_nom(n);
        om_nom_nom(m);
        println!("{a}", a = m + n)
    }

    fn _124_128() {
        // At any given time, you can have either one mutable reference or any number of immutable references.
        // lifetime of a value starts when it's `created` and ends the `last time` it's used
        // rust forbids situations when reference lifetime is more than the value lifetime (no dangling references)
        // rust calls the "drop" on a value when its lifetime ends
    }

    fn _132() {
        let x = Box::new(92);
        let _y;
        let _z;

        if rand::random() {
            _y = x;
        } else {
            _z = x;
        }
    }

    fn _133() {
        // rust checks program flow

        let mut x: i32;

        // used binding `x` isn't initialized
        // assert_eq!(x, 42)

        // first mutable flow starts
        x = 42;

        // first immutable flow starts
        let y = &x;

        // second mutable flow starts - illegal
        // cannot assign to `x` because it is borrowed
        // borrowed by y
        // x = 43;

        assert_eq!(*y, 42);
    }

    fn _134() {
        // has Copy
        let x1 = 42;
        // doesn't have Copy
        let y1 = Box::new(84);
        {
            // x1 is copied into z
            // y1 is moved into z
            let _z = (x1, y1);
            // lifetime of z ends
            // copy of x1 is dropped
            // y1 is dropped
        }

        let _x2 = x1;

        // y1 was dropped and can't be accessed
        // let y2 = y1
    }

    pub fn main() {
        _133()
    }
}

pub mod lecture2 {
    // https://www.youtube.com/watch?v=WM0St1vX_JM&list=PL4_hYwCyhAvbeLzi699gqMUA4UaPkcdmJ&index=2

    use std::{
        collections::{BTreeMap, BTreeSet, BinaryHeap, HashMap, HashSet, LinkedList, VecDeque},
        fmt::Debug,
        io::Write,
        rc::Rc,
    };

    fn _5() {
        let result = Some("string");
        match result {
            Some(s) => println!("String inside: {s}"),
            None => println!("Oops, no value"),
        }
    }

    fn _7() {
        // unwrap returns the contained Some value, consuming the self value.

        let opt = Some(22022022);
        assert!(opt.is_some());
        assert!(!opt.is_none());
        assert_eq!(opt.unwrap(), 22022022);

        // unwrap consumes `self`
        // number is Copy
        let _x = opt.unwrap();

        // can unwrap second time
        let _x = opt.unwrap();

        let _newest_opt: Option<i32> = None;
        // newest_opt.expect("I'll panic");

        let new_opt = Some(Vec::<i32>::new());

        assert_eq!(new_opt.unwrap(), Vec::<i32>::new());

        // vector is Clone, not Copy
        // can't unwrap second time
        // error[E0382]: use of moved value: `new_opt`
        // let x = new_opt.unwrap();
    }

    fn _8() {
        // as_ref Converts from &Option<T> to Option<&T>

        let new_opt = Some(Vec::<i32>::new());
        assert_eq!(new_opt.unwrap(), Vec::<i32>::new());

        // Clone => value already moved in the previous unwrap
        // let x = new_opt.unwrap();

        let new_opt = Some(Vec::<i32>::new());
        assert_eq!(new_opt.as_ref().unwrap(), &Vec::<i32>::new());
        let _x = new_opt.unwrap();

        // cannot mutate immutable variable `new_opt`
        // let y = new_opt.as_mut();
    }

    fn _9() {
        // map maps an Option<T> to Option<U> by applying a function to a contained value (if Some) or returns None (if None).
        // consumes self

        let maybe_some_string = Some(String::from("Hello, world"));
        let maybe_some_len = maybe_some_string.map(|s| s.len());
        assert_eq!(maybe_some_len, Some(13));
    }

    fn _10() {
        // Use Option methods to write functional code
        // https://doc.rust-lang.org/std/option/enum.Option.html#implementations

        // fn map_or<U, F>(self, default: U, f: F) -> U;
        // fn map_or_else<U, D, F>(self, default: D, f: F) -> U;
        // fn unwrap_or(self, default: T) -> T;
        // fn unwrap_or_else<F>(self, f: F) -> T;
        // fn and<U>(self, optb: Option<U>) -> Option<U>;
        // fn and_then<U, F>(self, f: F) -> Option<U>;
        // fn or(self, optb: Option<T>) -> Option<T>;
        // fn or_else<F>(self, f: F) -> Option<T>;
        // fn xor(self, optb: Option<T>) -> Option<T>;
        // fn zip<U>(self, other: Option<U>) -> Option<(T, U)>;
    }

    fn _11() {
        // Controlling ownership inside Option

        let mut x = Some(42);
        // `y` takes ownership from `x`, `x` becomes `None`
        let _y = x.take();

        let mut x = Some(42);

        // `x` has `Some(43)`, `y` has `Some(42)`
        let _y = x.replace(43);

        // `x` now has a mutable reference to `43`
        let _x = Some(42).insert(43);
    }

    fn _12() {
        struct Node<T> {
            elem: T,
            next: Option<Box<Node<T>>>,
        }

        pub struct List<T> {
            head: Option<Box<Node<T>>>,
        }

        impl<T: Debug> List<T> {
            pub fn pop(&mut self) -> Option<T> {
                self.head.take().map(|node| {
                    self.head = node.next;
                    node.elem
                })
            }
            pub fn push(&mut self, elem: T) -> () {
                self.head = Some(Box::new(Node {
                    elem,
                    next: self.head.take(),
                }));
            }
            pub fn push_(mut self, elem: T) -> List<T> {
                self.head = Some(Box::new(Node {
                    elem,
                    next: self.head,
                }));
                self
            }
            pub fn new() -> List<T> {
                List { head: None }
            }
            pub fn display(&mut self) -> () {
                print!("[");
                let mut i = &self.head;
                while let Some(x) = i {
                    print!("{:?}, ", x.elem);
                    i = &x.next;
                }
                print!("]\n");
            }
        }

        let mut t = List::<i32>::new().push_(2);
        t.push(4);

        t.display();

        let p1 = t.pop();
        let p2 = t.pop();
        let p3 = t.pop();

        println!("{p1:?} {p2:?} {p3:?}")
    }

    fn _13() {
        // Rust guarantees size(T) = size(F), where F:
        // Box<T>, &T, &mut T, fn, extern "C" fn, #[repr(transparent)], num::NonZero*, ptr::NonNull<T>
    }

    fn _14() {
        // Results must be used
        // errors must be handled
        // result is annotated with the #[must_use] attribute
    }

    fn _15() {
        let version: Result<&str, &str> = Ok("1.1.14");
        match version {
            Ok(v) => println!("working with version: {a:?}", a = v),
            Err(_e) => println!("error: version empty"),
        }
    }

    fn _16() {
        // Use Result methods to write functional code
        // https://doc.rust-lang.org/std/result/enum.Result.html#implementations

        // fn is_ok(&self) -> bool;
        // fn is_err(&self) -> bool;
        // fn unwrap(self) -> T;
        // fn unwrap_err(self) -> E;
        // fn expect_err(self, msg: &str) -> E;
        // fn expect(self, msg: &str) -> T;
        // fn as_ref(&self) -> Result<&T, &E>;
        // fn as_mut(&mut self) -> Result<&mut T, &mut E>;
        // fn map<U, F>(self, op: F) -> Result<U, E>;
        // fn map_err<F, O>(self, op: O) -> Result<T, F>;
    }

    pub mod _17 {
        pub struct Info {
            pub name: String,
            pub age: i32,
        }
    }

    fn _17_18() {
        use std::{fs, io};
        use _17::*;

        fn write_info(info: &Info) -> io::Result<()> {
            let mut file = match fs::File::create("my_best_friends.txt") {
                Err(e) => return Err(e),
                Ok(f) => f,
            };
            if let Err(e) = file.write_all(format!("name: {}\n", info.name).as_bytes()) {
                // can't use just Err(e) since it's not the last statement in the function `write_info`
                return Err(e);
            }
            if let Err(e) = file.write_all(format!("age: {}\n", info.age).as_bytes()) {
                return Err(e);
            }
            Ok(())
        }
    }

    fn _19() {
        // Refactored

        use std::{fs, io};
        use _17::*;

        fn write_info(info: &Info) -> io::Result<()> {
            // if error, ? returns an error, otherwise the value from Ok(value)
            // ? also works for option
            let mut file = fs::File::create("my_best_friends.txt")?;
            file.write_all(format!("name: {}\n", info.name).as_bytes())?;
            file.write_all(format!("age: {}\n", info.age).as_bytes())?;
            Ok(())
        }
    }

    fn _20() {
        let _x: Result<Option<i32>, String> = Some(Ok(42)).transpose();

        let _x: Option<Result<i32, String>> = Ok(Some(42)).transpose();
    }

    fn _21_22() {
        use std::{io, io::*};
        fn read_until_empty() -> io::Result<String> {
            let mut input = stdin().lines();
            let mut output = String::new();

            while let Some(line) = input.next().transpose()? {
                if line.is_empty() {
                    break;
                }
                output.push_str(&line);
            }

            println!("{output}");

            Ok(output)
        }

        let _ = read_until_empty();
    }

    fn _24() {
        // Containers

        // - don't allocate until necessary
        // - safe methods return Option and Result
        // - some methods panic! on failed allocation, out-of-bounds
        // - references to elements, no iterators like in C++

        // these properties affect algorithms and data structures used in the standard library
    }

    fn _26() {
        // Vec

        // implementation of default dynamic array

        // `sort` - O(NlogN)
        // `sort_unstable` - pdqsort, worst-case O(NlogN) and best O(N)
        // `binary_search`
        // `select_nth_unstable` - quick select with pdqsort, worst-case O(N)
        // `_by` and `_by_key` variants for customizing comparator
    }

    fn _27() {
        // VecDeque

        // circular deque
        // same functions as in Vec
        // stored in memory contiguously
        let mut t: VecDeque<i32> = VecDeque::new();
        t.push_back(2);
        t.push_front(4);
        println!("{t:?}");
    }

    fn _32() {
        // BTreeMap, BTreeSet - O (log_{B}{N}) for most operations
        let _x = BTreeMap::<i32, i32>::new();
        let _x = BTreeSet::<i32>::new();
    }

    fn _34() {
        // HashMap, HashSet

        let _x = HashMap::<i32, i32>::new();
        let _x = HashSet::<i32>::new();
        // it's a logic error to modify a map key such that it's hash or equality changes
        // behavior from such an error is unspecified, but not undefined
    }

    fn _36() {
        // BinaryHeap
        let _x = BinaryHeap::<i32>::new();
    }

    fn _37() {
        let _x = LinkedList::<i32>::new();
    }

    fn _38() {
        // String

        // UTF-8
        // contains a vector of bytes

        // consumes a vector to avoid allocations
        let _p = String::from_utf8(vec![3; 5]);
    }

    fn _42() {
        let s = String::from("привет");
        // length in bytes
        println!("{}", s.len())
    }

    fn _43() {
        let s = String::from("привет");
        let t = s.chars().collect::<Vec<_>>();
        println!("{:?}", t);
    }

    fn _44() {
        // char is a single character
        // char is a Unicode code point
        // https://codepoints.net
    }

    fn _45() {
        let mut chars = "é".chars();

        // https://codepoints.net/U+00E9
        // U+00E9 Latin Small Letter E with Acute
        assert_eq!(Some('\u{00E9}'), chars.next());
        assert_eq!(None, chars.next());
    }

    fn _46() {
        // char takes 4 bytes
        assert_eq!(std::mem::size_of::<char>(), 4);
    }

    fn _47() {
        // &str - slice of a string

        let vec = vec![1, 2, 3, 4];

        // &[2,3]
        let _vec_slice = &vec[1..3];

        let s = String::from("hello");

        // "el"
        let _s_slice = &s[1..3];
    }

    fn _48_49() {
        let s = String::from("привет");

        // &str checks at runtime

        // thread 'main' panicked at 'byte index 1 is not a char boundary; it is inside 'п' (bytes 0..2) of `привет`'
        let _s_slice = &s[1..3];
    }

    fn _52() {
        let s: &str = "Hello world!";
        let t1 = s.to_string();

        // same as t1
        let t2 = t1.to_owned();

        println!("{t1}\n{t2}")
    }

    fn _54() {
        // Box

        // fn leak<'a>(b: Box<T, A>) -> &'a mut T;
        // fn into_raw(b: Box<T, A>) -> *mut T;

        let x = Box::new(41);

        // static lifetime - leaves the whole program
        let static_ref: &'static mut usize = Box::leak(x);
        *static_ref += 1;
        assert_eq!(*static_ref, 42);
    }

    fn _55() {
        // it's safe to leak memory
        // e.g., allocate it for other programs
    }

    fn _56() {
        // single-threaded reference-counting pointer

        let rc = Rc::new(());

        // clones Rc, not content
        let _rc2 = rc.clone();

        // same
        let _rc3 = Rc::clone(&rc);

        // Rc is dropped when all instances of Rc are dropped

        // returns a mutable reference if there's only one pointer

        // fn get_mut(this: &mut Rc<T>) -> Option<&mut T>;
        // fn downgrade(this: &Rc<T>) -> Weak<T>;
        // fn weak_count(this: &Rc<T>) -> usize;
        // fn strong_count(this: &Rc<T>) -> usize;
    }

    fn _57() {
        let mut rc = Rc::new(42);
        println!("{}", *rc);

        *Rc::get_mut(&mut rc).unwrap() -= 41;
        println!("{}", *rc);

        let mut rc1 = rc.clone();
        println!("{}", rc1);

        // can't get a mutable reference because there's a pointer rc1
        // thread 'main' panicked at 'called `Option::unwrap()` on a `None` value'
        *Rc::get_mut(&mut rc1).unwrap() -= 1;
    }

    fn _58() {
        // Rc is a strong pointer
        // Weak is a weak pointer
        // Both of them have ownership over `allocation`
        // Only Rc has ownership over the `value` inside

        // If there are Rc and Weak and all Rc are dropped, the value is dropped, but allocation lives
        // If all Weak are dropped, there happens deallocation

        // Can upgrade Weak to Rc:
        // fn upgrade(&self) -> Option<Rc<T>>;
    }

    fn _59() {
        let rc1 = Rc::new(String::from("string"));

        // create another Rc
        let rc2 = rc1.clone();

        // make a weak pointer to allocation
        let weak1 = Rc::downgrade(&rc1);
        // make another weak pointer to allocation
        let weak2 = Rc::downgrade(&rc1);

        // rc2 remains => the string isn't dropped
        drop(rc1);

        // check that there's a value
        assert!(weak1.upgrade().is_some());

        drop(weak1);

        // the string is dropped
        drop(rc2);

        assert_eq!(weak2.strong_count(), 0);

        // If no strong pointers remain, this will return zero.
        // https://github.com/rust-lang/rust/pull/65778#issuecomment-549212322
        assert_eq!(weak2.weak_count(), 0);

        assert!(weak2.upgrade().is_none());

        drop(weak2);
    }

    fn _60() {
        // Arc - a thread-safe reference-counting pointer

        // sharing data safely across threads

        // It's possible to make a cycle on Rc and get a memory leak
        // memory won't be deallocated
    }

    pub fn main() {
        _59()
    }
}

pub mod lecture3 {

    pub mod _4 {
        pub trait Animal {
            fn name(&self) -> String;
            fn noise(&self) -> String;

            fn talk(&self) {
                println!("{} says {}", self.name(), self.noise());
            }
        }
    }

    fn _5_6() {
        use _4::*;

        pub struct Sheep {
            name: String,
        }

        impl Animal for Sheep {
            // no pub
            fn name(&self) -> String {
                self.name.clone()
            }

            fn noise(&self) -> String {
                "baaaah!".to_string()
            }
        }

        let sheep = Sheep {
            name: "Dolly".to_string(),
        };

        assert_eq!(sheep.name(), "Dolly");
        sheep.talk();
    }

    fn _7() {
        use _4::*;

        pub struct Dog {
            name: String,
        }

        impl Animal for Dog {
            fn name(&self) -> String {
                self.name.clone()
            }
            fn noise(&self) -> String {
                "ruff!".to_string()
            }

            // override default trait method
            fn talk(&self) {
                println!("Ruff! Don't call me doggo")
            }
        }

        let dog = Dog {
            name: "Dog".to_string(),
        };

        dog.talk()
    }

    fn _8() {
        use _4::*;

        #[derive(Clone)]
        pub struct Human {
            name: String,
        }

        impl Animal for Human {
            fn name(&self) -> String {
                self.name.clone()
            }

            fn noise(&self) -> String {
                let cloned = self.clone();
                cloned.name()
            }

            fn talk(&self) {
                println!("My name is {}", self.name())
            }
        }

        let human = Human {
            name: "Aristotle".to_string(),
        };

        human.talk()
    }

    fn _9() {
        pub trait Animal {
            fn name(&self) -> String;
            fn noise(&self) -> String;
            fn talk(&self) {
                // read-only references have Copy
                // This clones &Self, not Self!
                // let cloned = self.clone();

                // We didn't require Clone for Self, so we don't get it

                // error: no method named `clone` found for type aprameter `Self` in the current scope
                // let cloned = (*self).clone()

                println!("{} says {}", self.name(), self.noise());
            }
        }
    }

    fn _10() {
        pub trait Animal
        where
            Self: Clone,
        {
            fn name(&self) -> String;
            fn noise(&self) -> String;
            fn talk(&self) {
                // This clones Self, not &Self!
                let cloned = self.clone();
                println!("{} says {}", cloned.name(), cloned.noise());
            }
        }
    }

    fn _12() {
        // Trait bounds

        use core::hash::Hash;

        trait Strange1<T: Clone + Hash + Iterator>
        where
            T::Item: Clone,
        {
            fn new() -> Self;
        }

        trait Strange2<T>
        where
            T: Clone + Hash + Iterator,
            T::Item: Clone,
        {
            fn new() -> Self;
        }
    }

    fn _13() {
        // Rust doesn't have "inheritance"
        // Can make supertrait

        trait Person {
            fn name(&self) -> String;
        }

        // Person is a supertrait of Student
        // Implementing Student requires you to also impl Person
        trait Student: Person {
            fn university(&self) -> String;
        }
        trait Programmer {
            fn fav_language(&self) -> String;
        }
        trait CompSciStudent: Programmer + Student {
            fn git_usefname(&self) -> String;
        }
    }

    fn _14_15_16_17_18() {
        // Fully Qualified Syntax

        // Situation:
        // A type has multiple traits
        // Traits have methods with the same names

        struct Form {
            username: String,
            age: u8,
        }

        trait UsernameWidget {
            fn get(&self) -> String;
        }

        trait AgeWidget {
            fn get(&self) -> u8;
        }

        impl UsernameWidget for Form {
            fn get(&self) -> String {
                self.username.clone()
            }
        }

        impl AgeWidget for Form {
            fn get(&self) -> u8 {
                self.age
            }
        }

        let form = Form {
            username: "rustacean".to_string(),
            age: 28,
        };

        let username = UsernameWidget::get(&form);
        assert_eq!("rustacean".to_owned(), username);

        let age = <Form as AgeWidget>::get(&form);
        assert_eq!(28, age);
    }

    fn _20() {
        use core::hash::*;

        fn func<T: Hash + Clone>(_input: T) {}
    }

    fn _21() {
        use core::hash::*;

        // Syntax sugar
        fn func(_input: impl Hash + Clone) {}
    }

    fn _22_23() {
        // Methods depending on whether the type has implementations of some traits

        pub enum Option<T> {
            Dummy(T),
        }

        impl<T> Option<T> {
            pub fn unwrap_or_default(self) -> T
            where
                T: Default,
            {
                T::default()
            }
        }

        impl<T> Option<T> where T: Default {}
    }

    fn _25() {
        // Exotically sized types

        // "Regular"
        // Dynamically Sized Types, DST
        // Zero Sized Types, ZST
        // Empty Types
    }
    pub fn main() {
        _8()
    }
}
