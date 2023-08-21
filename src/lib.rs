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
    pub fn new(span: usize, progeny_values: Vec<i32>, mobility_values: Vec<i32>) -> Outcome {
        let mut progeny = BitVec::repeat(false, 2*span + 1);
        let mut mobility: HashSet<i32> = HashSet::new();

        progeny_values
            .iter()
            .for_each(|&i| {
                progeny.set((span as i32 + i) as usize, true);
            });

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

        for i in state.positions.iter() {
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
                        let a = universe.span() + i - self.span;
                        let b = i + self.span;

                        universe.cells.splice(a..universe.span(), progeny[..self.span-i].iter().by_vals());
                        universe.cells.splice(0..=b, progeny[self.span-i..].iter().by_vals());
                    },
                    j if j >= &(universe.span() - self.span) => {
                        let a = i - self.span;
                        let b = (i + self.span) % universe.span();
                        universe.cells.splice(a..universe.span(), progeny[..=universe.span()-i].iter().by_vals());
                        universe.cells.splice(0..=b, progeny[universe.span()-i+1..].iter().by_vals());
                    },
                    _ => {
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
    fn mobile_rule_apply() {
        let mut universe = Universe::new(11);

        let mut state = MobileState::new(universe.span());
        state.add_position(5);

        let mut rule: MobileRule = MobileRule::new(1);
        (0..8)
            .map(|_i| {
                let progeny: Vec<i32> = Vec::from([-1, 0, 1]);
                let mobility: Vec<i32> = Vec::from([1]);

                Outcome::new(1, progeny, mobility)
            })
            .enumerate()
            .for_each(|(i, outcome)| {
                rule.set_outcome(i, outcome);
            });


        rule.apply(&mut state, &mut universe);
        assert_eq!(&universe.cells[..], bits![
            0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0
        ]);
        assert_eq!(&state.positions.iter().collect::<Vec<&usize>>(), &Vec::from([&6]));
    }

    #[test]
    fn mobile_rule_apply_left() {
        let mut universe = Universe::new(11);

        let mut state = MobileState::new(universe.span());
        state.add_position(0);

        let mut rule: MobileRule = MobileRule::new(1);
        (0..8)
            .map(|_i| {
                let progeny: Vec<i32> = Vec::from([-1, 0, 1]);
                let mobility: Vec<i32> = Vec::from([-1]);

                Outcome::new(1, progeny, mobility)
            })
            .enumerate()
            .for_each(|(i, outcome)| {
                rule.set_outcome(i, outcome);
            });

        rule.apply(&mut state, &mut universe);
        assert_eq!(&universe.cells[..], bits![
            1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1
        ]);
        assert_eq!(&state.positions.iter().collect::<Vec<&usize>>(), &Vec::from([&10]));
    }

    #[test]
    fn mobile_rule_apply_right() {
        let mut universe = Universe::new(11);

        let mut state = MobileState::new(universe.span());
        state.add_position(10);

        let mut rule: MobileRule = MobileRule::new(1);
        (0..8)
            .map(|_i| {
                let progeny: Vec<i32> = Vec::from([-1, 0, 1]);
                let mobility: Vec<i32> = Vec::from([1]);

                Outcome::new(1, progeny, mobility)
            })
            .enumerate()
            .for_each(|(i, outcome)| {
                rule.set_outcome(i, outcome);
            });

        rule.apply(&mut state, &mut universe);
        assert_eq!(&universe.cells[..], bits![
            1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1
        ]);
        assert_eq!(&state.positions.iter().collect::<Vec<&usize>>(), &Vec::from([&0]));
    }

    #[test]
    fn mobile_rule_generalized_apply() {
        let mut universe = Universe::new(11);

        let mut state = MobileState::new(universe.span());
        state.add_position(5);
        state.add_position(0);

        let mut rule: MobileRule = MobileRule::new(1);
        (0..8)
            .map(|_i| {
                let progeny: Vec<i32> = Vec::from([-1, 0, 1]);
                let mobility: Vec<i32> = Vec::from([-1, 1]);

                Outcome::new(1, progeny, mobility)
            })
            .enumerate()
            .for_each(|(i, outcome)| {
                rule.set_outcome(i, outcome);
            });

        rule.apply(&mut state, &mut universe);
        assert_eq!(&universe.cells[..], bits![
            1, 1, 0, 0, 1, 1, 1, 0, 0, 0, 1
        ]);
        let mut new_positions: HashSet<usize> = HashSet::new();
        new_positions.insert(1);
        new_positions.insert(4);
        new_positions.insert(6);
        new_positions.insert(10);
        assert_eq!(&state.positions, &new_positions);
    }
}
