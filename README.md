Hyperbole
=========




https://github.com/seanhess/web-view/blob/57bbfe4046f7fdfffe49c4d01d3254317a113e1d/example/Main.hs#L12-L15






Reference CSS Layout that Worked
--------------------------------

    /* Wookie.UI */

    html, body {
      height: 100%;
      font-family: -apple-system, BlinkMacSystemFont, Segoe UI, Roboto, Helvetica, Arial, sans-serif;
      -webkit-font-smoothing: antialiased;
    }

    .a {
        position: relative;
        border: none;
        flex-shrink: 0;
        display: flex;
        flex-direction: row;
        flex-basis: auto;
        resize: none;
        font-feature-settings: inherit;
        box-sizing: border-box;
        margin: 0;
        padding: 0;
        border-width: 0;
        border-style: solid;
        font-size: inherit;
        color: inherit;
        font-family: inherit;
        line-height: 1;
        font-weight: inherit;
        text-decoration: none;
        font-style: inherit;

        /* wc = width content */
        align-self: flex-start;
    }

    .r {
      display: flex;
      flex-direction: row;
    }

    .c {
      display: flex;
      flex-direction: column;
    }

    .a.r > .a {
      flex-basis: 0%;
      min-height: min-content;
    }

    .a.c > .a {
      flex-basis: 0px;
      min-height: min-content;
    }

    .a.c > .wf {
      width: 100%;
    }

    .a.c > .hf {
      flex-grow: 100000;
    }

    .a.r > .wf {
      flex-grow: 100000;
    }

    .a.r > .hf {
      align-self: stretch !important;
    }

    .el {

    }

    .layout {
      display: flex;
      flex-direction: column;
      white-space: pre;
      width: 100%;
      height: 100%;
      min-height: 100%;
      z-index: 0;
    }

