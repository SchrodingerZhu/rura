use rura_core::ast::ModuleID;

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
    "#;
    let m = module(INPUT, ModuleID::crate_name("test")).unwrap();
    assert_eq!(m.id.to_string(), "test");
    assert!(!m.declarations.is_empty())
}
