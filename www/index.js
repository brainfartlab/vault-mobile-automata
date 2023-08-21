import * as wasm from "vault-mobile-automata";
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

customElements.define('mobile-automata',
  class extends HTMLElement {
    constructor() {
      super();
    }

    connectedCallback() {
    }

    attributeChangedCallback(name, oldValue, newValue) {
      console.log(`${name} changed to ${newValue}`);
    }

    static get observedAttributes() {
      return ['rule', 'cell-size', 'state'];
    }
  });
