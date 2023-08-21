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

const getTapeDimensions = (width, height, cellSize) => {
  let span = Math.floor((width - 1) / (cellSize + 1));
  let depth = Math.floor((height - 1) / (cellSize + 1));

  return { span, depth };
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
      console.log(`${combination}: progeny = ${progeny}, mobility = ${mobility}`);
      rule.set_outcome(combination, outcome);
    });

    return rule;
  } else {
    return MobileRule.new(windowSpan);
  }

  return rule;
}

class MobileAutomata {
  constructor(span, rule, windowSpan) {
    this.universe = Universe.new(span);
    this.rule = rule;
    this.state = MobileState.new(span);

    this.iteration = 0;
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

customElements.define('mobile-automata',
  class extends HTMLElement {
    constructor() {
      super();

      this.canvas = this.querySelector('#ma-canvas');

      this.cellSize = 10;
      let { span, depth } = getTapeDimensions(this.canvas.width, this.canvas.height, this.cellSize);
      this.depth = depth;

      console.log(`constructor width: ${this.canvas.width}`);
      this.ca = new MobileAutomata(span, null, 1);

      this.animationId = null;
    }

    connectedCallback() {
      this.reset();
    }

    resize() {
      let { span, depth } = getTapeDimensions(this.canvas.width, this.canvas.height, this.cellSize);
      this.depth = depth;

      this.ca = new MobileAutomata(span, this.ca.rule, 1);
      this.reset();
    }

    reset() {
      this.ca.clearCells();
      this.ca.clearStates();

      let middle = Math.floor(this.ca.universe.span() / 2);
      // this.ca.setCell(middle, true);
      this.ca.setState(middle);

      this.canvas.getContext('2d').clearRect(0, 0, this.canvas.width, this.canvas.height);

      drawGrid(this.canvas.getContext('2d'), this.cellSize, this.ca.universe.span(), this.depth);
      drawIteration(this.canvas.getContext('2d'), this.cellSize, this.depth, this.ca);
    }

    attributeChangedCallback(name, oldValue, newValue) {
      switch (name) {
        case 'rule':
          this.ca.setRule(newValue, 1);
          this.reset();
          break;

        case 'cell-size':
          this.cellSize = parseInt(newValue);

          let { span, depth } = getTapeDimensions(this.canvas.width, this.canvas.height, this.cellSize);
          this.depth = depth;
          this.ca.wipe(span);

          this.reset();
          break;

        case 'state':
          this.changeState(newValue);
          break;

        case 'canvas-width':
          console.log(`Changing canvas width: ${newValue}`);
          this.canvas.width = parseInt(newValue);
          this.resize();
          break;

        case 'canvas-height':
          console.log('Changing canvas height');
          this.canvas.height = parseInt(newValue) - 4;
          this.resize();
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
      return ['rule', 'cell-size', 'state', 'canvas-width', 'canvas-height'];
    }

    play() {
      this.renderLoop();
    }

    pause() {
      cancelAnimationFrame(this.animationId);
    }

    renderLoop() {
      this.ca.tick();
      drawIteration(this.canvas.getContext('2d'), this.cellSize, this.depth, this.ca);

      this.animationId = requestAnimationFrame(this.renderLoop.bind(this));
    }
  }
);

function drawGrid(ctx, cellSize, span, depth) {
  ctx.beginPath();
  ctx.strokeStyle = COLOR_GRID;
  ctx.strokeWidth  =1;

  // vertical lines
  for (let i = 0; i <= span; i++) {
    let x = i * (cellSize + 1) + 1;

    ctx.moveTo(x, 0);
    ctx.lineTo(x, depth * (cellSize + 1));
  }

  // horizontal lines
  for (let i = 0; i <= depth; i++) {
    let y = i * (cellSize + 1) + 1;

    ctx.moveTo(0, y);
    ctx.lineTo(span * (cellSize + 1), y);
  }

  ctx.stroke();
}

function drawIteration(ctx, cellSize, depth, ca) {
  const cellsPtr = ca.universe.as_ptr();
  const cells = new Uint8Array(memory.buffer, cellsPtr, Math.ceil(ca.universe.span() / 8));

  ctx.beginPath();

  let layer = ca.iteration % depth;
  let yOffset = layer * (cellSize + 1) + 1;

  for (let c = 0; c <= ca.universe.span(); c++) {
    ctx.fillStyle = isBitSet(c, cells)
      ? COLOR_ALIVE
      : COLOR_DEAD;

    ctx.fillRect(
      c * (cellSize + 1) + 1,
      yOffset,
      cellSize,
      cellSize,
    );
  }

  ctx.fillStyle = COLOR_STATE;
  let positions = ca.state.get_positions();
  positions.forEach((position) => {
    ctx.arc(
      position * (cellSize + 1) + 1 + (cellSize / 2),
      yOffset + (cellSize / 2),
      cellSize / 4,
      0,
      2 * Math.PI,
    );
  });

  // ctx.stroke();
  ctx.fill();
}
