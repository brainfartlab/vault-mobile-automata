mod utils;

extern crate bitvec;

use bitvec::prelude::*;
use std::collections::HashSet;
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub struct Universe {
    cells: BitVec,
}

#[wasm_bindgen]
impl Universe {
    pub fn new(span: usize) -> Universe {
        let cells = BitVec::repeat(false, span);

        Universe {
            cells,
        }
    }

    pub fn span(&self) -> usize {
        self.cells.len()
    }

    pub fn set(&mut self, index: usize, value: bool) {
        self.cells.set(index, value);
    }

    pub fn get(&self, index: usize) -> bool {
        self.cells[index]
    }

    pub fn as_ptr(&self) -> *const usize {
        self.cells.as_bitptr().pointer()
    }

    pub fn count_alive(&self) -> usize {
        self.cells.as_bitslice().count_ones()
    }
}

trait State {}

#[wasm_bindgen]
pub struct MobileState {
    positions: HashSet<usize>
}

#[wasm_bindgen]
impl MobileState {
    pub fn new(span: usize) -> MobileState {
        let positions = HashSet::with_capacity(span);

        MobileState {
            positions,
        }
    }

    pub fn add_position(&mut self, position: usize) {
        self.positions.insert(position);
    }

    pub fn contains(&self, position: usize) -> bool {
        self.positions.contains(&position)
    }

    pub fn get_positions(&self) -> Vec<usize> {
        self.positions.iter().map(|i| *i).collect()
    }

    fn update_positions(&mut self, positions: HashSet<usize>) {
        self.positions = positions;
    }
}

impl State for MobileState {}

trait Rule<T: State> {
    fn apply(&self, state: &mut T, universe: &mut Universe);
}

#[wasm_bindgen]
pub struct Outcome {
    progeny: BitVec,
    mobility: HashSet<i32>,
}

#[wasm_bindgen]
impl Outcome {
    pub fn new(progeny_values: Vec<JsValue>, mobility_values: Vec<i32>) -> Outcome {
        let mut progeny = BitVec::repeat(false, progeny_values.len());

        progeny_values
            .iter()
            .map(|v| {
                v.as_bool().unwrap()
            })
            .enumerate()
            .for_each(|(i, b)| {
                progeny.set(i, b);
            });

        let mut mobility: HashSet<i32> = HashSet::new();

        mobility_values
            .iter()
            .for_each(|&i| {
                mobility.insert(i);
            });

        Outcome {
            progeny,
            mobility,
        }
    }

    fn from_bitvec(span: usize, progeny: BitVec, mobility_values: Vec<i32>) -> Outcome {
        let mut mobility: HashSet<i32> = HashSet::new();

        mobility_values
            .iter()
            .for_each(|&i| {
                mobility.insert(i);
            });

        Outcome {
            progeny,
            mobility,
        }
    }
}

#[wasm_bindgen]
pub struct MobileRule {
    span: usize,
    cases: Vec<Outcome>,
}

#[derive(Debug)]
pub struct RuleError {}

#[wasm_bindgen]
impl MobileRule {
    pub fn new(span: usize) -> MobileRule {
        let num_cases = i32::pow(2, 2*(span as u32) + 1);
        let cases: Vec<Outcome> = (0..num_cases)
            .map(|_i| {
                Outcome {
                    progeny: BitVec::repeat(false, 1),
                    mobility: HashSet::new(),
                }
            })
            .collect();

        MobileRule {
            span,
            cases,
        }
    }

    pub fn set_outcome(&mut self, index: usize, outcome: Outcome) {
        self.cases[index] = outcome;
    }
}

impl Rule<MobileState> for MobileRule {
    fn apply(&self, state: &mut MobileState, universe: &mut Universe) {
        let a = universe.span() - self.span;
        let mut new_positions: HashSet<usize> = HashSet::new();

        // iterate over all states
        for i in state.positions.iter() {
            // construct the combination at this state as an int
            let combination: usize = (a..=a+2*self.span)
                .map(|j| {
                    let index = (i + j) % universe.span();
                    universe.cells[index]
                })
                .rev()
                .enumerate()
                .map(|(i, val)| {
                    (val as usize) << i
                })
                .sum();

            let outcome = &self.cases[combination];

            // progeny
            let progeny = &outcome.progeny;
            if progeny.len() > 1 {
                // extended
                match i {
                    j if j < &self.span => {
                        // the state is within 0..span
                        let a = universe.span() + i - self.span;
                        let b = i + self.span;

                        universe.cells.splice(a..universe.span(), progeny[..self.span-i].iter().by_vals());
                        universe.cells.splice(0..=b, progeny[self.span-i..].iter().by_vals());
                    },
                    j if j >= &(universe.span() - self.span) => {
                        // the state is within k-span..k, with k the length of the universe
                        let a = i - self.span;
                        let b = (i + self.span) % universe.span();
                        universe.cells.splice(a..universe.span(), progeny[..=universe.span()-i].iter().by_vals());
                        universe.cells.splice(0..=b, progeny[universe.span()-i+1..].iter().by_vals());
                    },
                    _ => {
                        // the state is within span+1..k-span-1
                        universe.cells.splice(i-self.span..=i+self.span, progeny.iter().by_vals());
                    },
                }
            } else {
                // simple or generalized
                universe.cells.splice(i..=i, progeny.iter().by_vals());
            }

            // positions
            let next_positions: HashSet<usize> = outcome.mobility
                .iter()
                .map(|j| {
                    let b = (*i as i32) + j + universe.span() as i32;
                    (b as usize) % universe.span()
                })
                .collect();

            new_positions.extend(&next_positions);
        }

        state.update_positions(new_positions);
    }
}

#[wasm_bindgen]
pub fn iterate(rule: &MobileRule, universe: &mut Universe, state: &mut MobileState) {
    rule.apply(state, universe);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn rule_new() {
        let rule: MobileRule = MobileRule::new(1);
        assert_eq!(rule.cases.len(), 8);
    }

    #[test]
    fn mobile_state_new() {
        MobileState::new(6);
    }

    #[test]
    fn mobile_state_add_position() {
        let mut state = MobileState::new(6);
        state.add_position(2);
    }

    #[test]
    fn mobile_rule_simple_apply() {
        let mut universe = Universe::new(11);

        let mut state = MobileState::new(universe.span());
        state.add_position(0);
        state.add_position(5);
        state.add_position(10);

        let mut rule: MobileRule = MobileRule::new(1);
        (0..8)
            .map(|_i| {
                let progeny: BitVec = BitVec::repeat(true, 1);
                let mobility: Vec<i32> = Vec::from([-1]);

                Outcome::from_bitvec(1, progeny, mobility)
            })
            .enumerate()
            .for_each(|(i, outcome)| {
                rule.set_outcome(i, outcome);
            });


        iterate(&rule, &mut universe, &mut state);
        assert_eq!(&universe.cells[..], bits![
            1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1
        ]);
        assert!(state.contains(4));
        assert!(state.contains(9));
        assert!(state.contains(10));
    }

    #[test]
    fn mobile_rule_simple_apply2() {
        let mut universe = Universe::new(11);

        let mut state = MobileState::new(universe.span());
        state.add_position(5);

        let mut rule: MobileRule = MobileRule::new(1);
        rule.set_outcome(0, Outcome::from_bitvec(1, BitVec::repeat(true, 1), Vec::from([-1])));
        rule.set_outcome(1, Outcome::from_bitvec(1, BitVec::repeat(true, 1), Vec::from([1])));
        rule.set_outcome(2, Outcome::from_bitvec(1, BitVec::repeat(false, 1), Vec::from([1])));
        rule.set_outcome(3, Outcome::from_bitvec(1, BitVec::repeat(false, 1), Vec::from([-1])));
        rule.set_outcome(4, Outcome::from_bitvec(1, BitVec::repeat(false, 1), Vec::from([-1])));
        rule.set_outcome(5, Outcome::from_bitvec(1, BitVec::repeat(true, 1), Vec::from([-1])));
        rule.set_outcome(6, Outcome::from_bitvec(1, BitVec::repeat(false, 1), Vec::from([1])));
        rule.set_outcome(7, Outcome::from_bitvec(1, BitVec::repeat(false, 1), Vec::from([1])));


        iterate(&rule, &mut universe, &mut state);
        assert_eq!(&universe.cells[..], bits![
            0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0
        ]);
        assert!(state.contains(4));

        iterate(&rule, &mut universe, &mut state);
        assert_eq!(&universe.cells[..], bits![
            0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0
        ]);
        assert!(state.contains(5));

        iterate(&rule, &mut universe, &mut state);
        assert_eq!(&universe.cells[..], bits![
            0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0
        ]);
        assert!(state.contains(6));

        iterate(&rule, &mut universe, &mut state);
        assert_eq!(&universe.cells[..], bits![
            0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0
        ]);
        assert!(state.contains(5));
    }
}
