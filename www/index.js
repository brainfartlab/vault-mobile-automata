import { Universe, MobileRule, MobileState, Outcome, iterate } from "mobile-automata";
import { memory } from "mobile-automata/mobile_automata_bg";
import "./elm.js"

var app = Elm.Main.init({ node: document.querySelector('main') });

if ('serviceWorker' in navigator) {
  window.addEventListener('load', () => {
    navigator.serviceWorker.register('./sw.js').then(registration => {
      console.log('SW registered: ', registration);
    }).catch(registrationError => {
      console.log('SW registration failed: ', registrationError);
    });
  });
}

const COLOR_STATE = '#000000';
const COLOR_ALIVE = '#888888';
const COLOR_DEAD = '#ffffff';
const COLOR_GRID = '#cccccc';

const isBitSet = (n, arr) => {
  const byte = Math.floor(n / 8);
  const mask = 1 << (n % 8);
  return (arr[byte] & mask) === mask;
};

const combinationToIndex = (c) => {
  return c.reduce((acc, value) => 2*acc + value, 0);
};

const parseRule = (configuration) => {
  if (configuration !== null) {
    let ruleConfig = JSON.parse(configuration);
    let rule = MobileRule.new(ruleConfig.span);

    ruleConfig.cases.forEach((ruleCase) => {
      let combination = combinationToIndex(ruleCase.combination);
      let progeny = ruleCase.outcome.progeny.map((v) => (v > 0));
      let mobility = ruleCase.outcome.mobility;

      let outcome = Outcome.new(progeny, mobility);
      rule.set_outcome(combination, outcome);
    });

    return rule;
  } else {
    return MobileRule.new(windowSpan);
  }

  return rule;
}

class MobileAutomata {
  constructor(span, depth, rule, windowSpan) {
    this.universe = Universe.new(span);
    this.rule = rule;
    this.state = MobileState.new(span);

    this.iteration = 0;
    this.depth = depth;
  }

  setRule(configuration, windowSpan) {
    this.rule = parseRule(configuration, windowSpan);
  }

  setCell(index, active) {
    this.universe.set(index, active);
  }

  setState(index) {
    this.state.add_position(index);
  }

  clearCells() {
    this.wipe(this.universe.span());
  }

  wipe(span) {
    this.iteration = 0;
    this.universe = Universe.new(span);
  }

  clearStates() {
    this.state = MobileState.new(this.universe.span());
  }

  get span() {
    return this.universe.span();
  }

  tick() {
    iterate(this.rule, this.universe, this.state);
    this.iteration++;
  }
}

class Tape {
  constructor(canvas, gridCanvas, cellSize) {
    this.canvas = canvas;
    this.grid = gridCanvas;

    this.cellSize = cellSize;
    this.buffer = null;
  }

  get width() {
    return this.canvas.width;
  }

  set width(newWidth) {
    this.canvas.width = newWidth;
    this.grid.width = newWidth;

    this.clearBuffer();
  }

  get height() {
    return this.canvas.height;
  }

  set height(newHeight) {
    this.canvas.height = newHeight;
    this.grid.height = newHeight;

    this.clearBuffer();
  }

  get span() {
    return Math.floor((this.width - 1) / (this.cellSize + 1));
  }

  get depth() {
    return Math.floor((this.height - 1) / (this.cellSize + 1));
  }

  clearBuffer() {
    this.buffer = null;
  }

  clear() {
    this.canvas.getContext('2d').clearRect(0, 0, this.canvas.width, this.canvas.height);
  }

  get context() {
    return this.canvas.getContext('2d');
  }

  clearGrid() {
    this.grid.getContext('2d').clearRect(0, 0, this.grid.width, this.grid.height);
  }

  get gridContext() {
    return this.grid.getContext('2d');
  }

  save(offset) {
    this.buffer = this.context.getImageData(0, offset, this.width, this.height);
  }

  restore() {
    if (this.buffer !== null) {
      this.context.putImageData(this.buffer, 0, 0);
    }
  }
}

customElements.define('mobile-automata',
  class extends HTMLElement {
    constructor() {
      super();

      this.simulationSpeed = 1;
      let canvas = this.querySelector('#ma-canvas');
      let gridCanvas = document.createElement("canvas");

      this.tape = new Tape(canvas, gridCanvas, 10);
      this.ca = new MobileAutomata(this.tape.span, this.tape.depth, null, 1);

      this.animationId = null;
    }

    connectedCallback() {
      this.reset();
    }

    resize() {
      this.ca = new MobileAutomata(this.tape.span, this.tape.depth, this.ca.rule, 1);
      this.reset();
    }

    reset() {
      this.ca.clearCells();
      this.ca.clearStates();

      let middle = Math.floor(this.ca.universe.span() / 2);
      this.ca.setState(middle);

      this.tape.clearBuffer();

      drawGrid(this.tape);
      drawIteration(this.tape, this.ca);
    }

    attributeChangedCallback(name, oldValue, newValue) {
      switch (name) {
        case 'rule':
          this.ca.setRule(newValue, 1);
          this.reset();
          break;

        case 'cell-size':
          let cellSize = parseInt(newValue);
          this.tape.cellSize = cellSize;
          this.ca.wipe(this.tape.span);

          this.reset();
          break;

        case 'state':
          this.changeState(newValue);
          break;

        case 'canvas-width':
          this.tape.width = parseInt(newValue);
          this.resize();
          break;

        case 'canvas-height':
          this.tape.height = parseInt(newValue) - 4;
          this.resize();
          break;

        case 'simulation-speed':
          this.simulationSpeed = parseInt(newValue);
          break;
      }
    }

    changeState(state) {
      switch (state) {
        case 'paused':
          this.pause()
          break;

        case 'running':
          this.play();
          break;
      }
    }

    static get observedAttributes() {
      return ['rule', 'cell-size', 'state', 'canvas-width', 'canvas-height', 'simulation-speed'];
    }

    play() {
      this.renderLoop();
    }

    pause() {
      cancelAnimationFrame(this.animationId);
    }

    renderLoop() {
      for (let i = 0; i < this.simulationSpeed; i++) {
        this.ca.tick();

        drawIteration(this.tape, this.ca);
      }

      this.animationId = requestAnimationFrame(this.renderLoop.bind(this));
    }
  }
);

function drawGrid(tape) {
  tape.clearGrid();
  let ctx = tape.gridContext;

  ctx.beginPath();
  ctx.strokeStyle = COLOR_GRID;
  ctx.strokeWidth = 1;

  // vertical lines
  for (let i = 0; i <= tape.span; i++) {
    let x = i * (tape.cellSize + 1) + 1;

    ctx.moveTo(x, 0);
    ctx.lineTo(x, tape.depth * (tape.cellSize + 1));
  }

  // horizontal lines
  for (let i = 0; i <= tape.depth; i++) {
    let y = i * (tape.cellSize + 1) + 1;

    ctx.moveTo(0, y);
    ctx.lineTo(tape.span * (tape.cellSize + 1), y);
  }

  ctx.stroke();
}

function drawIteration(tape, ca) {
  tape.clear();

  tape.restore();

  const cellsPtr = ca.universe.as_ptr();
  const cells = new Uint8Array(memory.buffer, cellsPtr, Math.ceil(ca.universe.span() / 8));
  let ctx = tape.context;
  ctx.beginPath();

  let layer = ca.iteration >= tape.depth
    ? tape.depth - 1
    : ca.iteration;
  let yOffset = layer * (tape.cellSize + 1) + 1;

  for (let c = 0; c <= ca.universe.span(); c++) {
    ctx.fillStyle = isBitSet(c, cells)
      ? COLOR_ALIVE
      : COLOR_DEAD;

    ctx.fillRect(
      c * (tape.cellSize + 1) + 1,
      yOffset * (tape.cellSize + 1) + 1,
      tape.cellSize,
      tape.cellSize,
    );
  };

  ctx.fillStyle = COLOR_STATE;
  let positions = ca.state.get_positions();
  positions.forEach((p) => {
    ctx.moveTo(
      p * (tape.cellSize + 1) + 1,
      yOffset,
    );
    ctx.arc(
      p * (tape.cellSize + 1) + 1 + (tape.cellSize / 2),
      yOffset + (tape.cellSize / 2),
      tape.cellSize / 4,
      0,
      2 * Math.PI,
    );
  });

  ctx.fill();

  let offset = ca.iteration > tape.depth
    ? tape.cellSize + 1
    : 0;
  tape.save(offset);

  // draw grid
  tape.context.drawImage(tape.grid, 0, 0);
}
