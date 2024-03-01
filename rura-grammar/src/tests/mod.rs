use rura_core::ast::{Definition, Expression, ModuleID};
use rura_core::{Constant, Input};

use crate::{if_then_else, module};

#[test]
fn it_parses_empty() {
    let m = module("", ModuleID::crate_name("test")).unwrap();
    assert_eq!(m.id.to_string(), "test");
    assert!(m.declarations.is_empty())
}

#[test]
fn it_parses_braced_code() {
    const INPUT: &str = r#"
mod m1 {
    fn f1   () {
    match n {
            C1=>  {   y  .  a  }  ,
            // FIXME: Currently don't know how to tackle this lol.
            // FIXME: Why is this so slow on release mode?
            // C2 ( x ) => ( ( ( (x . a) :: < T   > () )  . b )   (x)  ) . 10 ,
             C3  =>  | lol | 1 + 2 * 3  ,
             C4 { z } =>  | oh | { ! z  } ,
        }
  }

    fn f2   < T >  () -> i32 {
        let n = if true {
            let x = 42;
            x
        } else {
            69
        };
    }
}

   mod m2   ;

    mod  m3 {
       mod m4 { }
}

enum  e1   <T> {
   C1,
      C2   (i32),
    C3 { f: f32 },
 }

struct   s1   <   T, U > {
  f1:  ()   ,
 f2   :i32
}
    "#;
    let m = module(INPUT, ModuleID::crate_name("test")).unwrap();
    assert_eq!(m.id.to_string(), "test");
    match &m.declarations[0].definition {
        Definition::NestedSubmodule(m) => assert_eq!(m.id.to_string(), "m1"),
        _ => assert!(false),
    }
    match &m.declarations[1].definition {
        Definition::ExternalSubmodule(m) => assert_eq!(m.to_string(), "m2"),
        _ => assert!(false),
    }
    match &m.declarations[2].definition {
        Definition::NestedSubmodule(m) => {
            assert_eq!(m.id.to_string(), "m3");
            match &m.declarations[0].definition {
                Definition::NestedSubmodule(m) => assert_eq!(m.id.to_string(), "m4"),
                _ => assert!(false),
            }
        }
        _ => assert!(false),
    }
    match &m.declarations[3].definition {
        Definition::Enum(d) => assert_eq!(d.name.as_str(), "e1"),
        _ => assert!(false),
    }
    match &m.declarations[4].definition {
        Definition::Struct(d) => assert_eq!(d.name.as_str(), "s1"),
        _ => assert!(false),
    }
}

#[test]
fn it_parses_if_expression() {
    let mut input = Input::new("if true { 42 } else { 69 }");
    let ast = if_then_else(&mut input).unwrap();
    let (c, t, e) = match ast.expr.as_ref() {
        Expression::IfThenElse {
            condition,
            then_branch,
            else_branch,
        } => (condition, then_branch, else_branch),
        _ => {
            assert!(false);
            unreachable!()
        }
    };
    match (c.expr.as_ref(), t.expr.as_ref(), e.expr.as_ref()) {
        (Expression::Constant(c), Expression::Constant(t), Expression::Constant(e)) => {
            match (c, t, e) {
                (Constant::Bool(c), Constant::I8(t), Constant::I8(e)) => {
                    assert!(c);
                    assert_eq!(*t, 42);
                    assert_eq!(*e, 69);
                }
                _ => {
                    assert!(false);
                    unreachable!()
                }
            }
        }
        _ => {
            assert!(false);
            unreachable!()
        }
    };
}
