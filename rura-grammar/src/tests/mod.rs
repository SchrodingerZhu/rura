use rura_core::ast::{Definition, ModuleID};

use crate::module;

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
    fn f1() {}
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
