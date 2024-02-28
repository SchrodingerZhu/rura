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
mod subtest {
}
    "#;
    let m = module(INPUT, ModuleID::crate_name("test")).unwrap();
    assert_eq!(m.id.to_string(), "test");
    assert!(!m.declarations.is_empty())
}
