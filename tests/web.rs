//! Test suite for the Web and headless browsers.

//#![cfg(target_arch = "wasm32")]

extern crate wasm_bindgen_test;
use vault_mobile_automata::*;
use wasm_bindgen::JsValue;
use wasm_bindgen_test::*;

// wasm_bindgen_test_configure!(run_in_worker);

#[wasm_bindgen_test]
fn mobile_rule_apply() {
    let mut universe = Universe::new(11);

    let mut state = MobileState::new(universe.span());
    state.add_position(5);

    let mut rule: MobileRule = MobileRule::new(1);
    (0..8)
        .map(|_i| {
            let progeny: Vec<JsValue> = Vec::from([JsValue::TRUE, JsValue::TRUE, JsValue::TRUE]);
            let mobility: Vec<i32> = Vec::from([1]);

            Outcome::new(1, progeny, mobility)
        })
        .enumerate()
        .for_each(|(i, outcome)| {
            rule.set_outcome(i, outcome);
        });


    iterate(&rule, &mut universe, &mut state);
    assert_eq!(universe.count_alive(), 3);
    // assert_eq!(&universe.cells[..], bits![
    //     0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0
    // ]);
    assert!(state.contains(6));
}

#[wasm_bindgen_test]
fn mobile_rule_apply_left() {
    let mut universe = Universe::new(11);

    let mut state = MobileState::new(universe.span());
    state.add_position(0);

    let mut rule: MobileRule = MobileRule::new(1);
    (0..8)
        .map(|_i| {
            let progeny: Vec<JsValue> = Vec::from([JsValue::TRUE, JsValue::TRUE, JsValue::TRUE]);
            let mobility: Vec<i32> = Vec::from([-1]);

            Outcome::new(1, progeny, mobility)
        })
        .enumerate()
        .for_each(|(i, outcome)| {
            rule.set_outcome(i, outcome);
        });

    iterate(&rule, &mut universe, &mut state);
    assert_eq!(universe.count_alive(), 3);
    // assert_eq!(&universe.cells[..], bits![
    //     1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1
    // ]);
    assert!(state.contains(10));
}

#[wasm_bindgen_test]
fn mobile_rule_apply_right() {
    let mut universe = Universe::new(11);

    let mut state = MobileState::new(universe.span());
    state.add_position(10);

    let mut rule: MobileRule = MobileRule::new(1);
    (0..8)
        .map(|_i| {
            let progeny: Vec<JsValue> = Vec::from([JsValue::TRUE, JsValue::TRUE, JsValue::TRUE]);
            let mobility: Vec<i32> = Vec::from([1]);

            Outcome::new(1, progeny, mobility)
        })
        .enumerate()
        .for_each(|(i, outcome)| {
            rule.set_outcome(i, outcome);
        });

    iterate(&rule, &mut universe, &mut state);
    assert_eq!(universe.count_alive(), 3);
    // assert_eq!(&universe.cells[..], bits![
    //     1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1
    // ]);
    assert!(state.contains(0));
}

#[wasm_bindgen_test]
fn mobile_rule_generalized_apply() {
    let mut universe = Universe::new(11);

    let mut state = MobileState::new(universe.span());
    state.add_position(5);
    state.add_position(0);

    let mut rule: MobileRule = MobileRule::new(1);
    (0..8)
        .map(|_i| {
            let progeny: Vec<JsValue> = Vec::from([JsValue::TRUE, JsValue::TRUE, JsValue::TRUE]);
            let mobility: Vec<i32> = Vec::from([-1, 1]);

            Outcome::new(1, progeny, mobility)
        })
        .enumerate()
        .for_each(|(i, outcome)| {
            rule.set_outcome(i, outcome);
        });

    iterate(&rule, &mut universe, &mut state);
    assert_eq!(universe.count_alive(), 6);
    // assert_eq!(&universe.cells[..], bits![
    //     1, 1, 0, 0, 1, 1, 1, 0, 0, 0, 1
    // ]);
    assert!(state.contains(1));
    assert!(state.contains(4));
    assert!(state.contains(6));
    assert!(state.contains(10));
}
