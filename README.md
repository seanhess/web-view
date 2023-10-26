Development
-----------

Load examples and library, and auto-reload

    ghcid --command="cabal repl example" --test main

Hyperbole! What a great name
HyperBOLE

HtmxBeatsOtherLameExperiences





<div class="row pad-20">
  <div class="bg-graylight pad-20">
    <div class="col" id="Contents">
      <div class="col gap-10 tt h-80">
        <div>Hello</div>
        <button class="bg-primary clr-white pad-10 hover:bg-primarylight" data-on-click="Expand" data-target="Contents">Expand</button>
      </div>
    </div>
  </div>
</div>


<div class="row pad-20">
  <div class="bg-graylight pad-20">
    <div class="col" id="Contents">
      <style type="text/css">.bg-primary { background-color:#2C74BB }
        .clr-white { color:#FFF }
        .col { display:flex; flex-direction:column }
        .gap-10 { gap:0.625rem }
        .h-220 { flex-shrink:0; height:13.75rem }
        .pad-10 { padding:0.625rem }
        .tt { transition-duration:0.5s; transition-property:height }
        .hover\:bg-primarylight:hover { background-color:#3281cf }</style>
    <div class="col gap-10 tt h-220">
      <div>One</div>
      <div>Two</div>
      <div>Three</div>
      <div>Four</div>
      <div>Five</div>
    <button class="bg-primary clr-white pad-10 hover:bg-primarylight" data-on-click="Collapse" data-target="Contents">Collapse</button>
  </div>
  </div>
  </div>
</div>


<div class="row pad-20">
  <div class="bg-graylight pad-20">
    <div class="col" id="Contents"><style type="text/css">.bg-primary { background-color:#2C74BB }
.clr-white { color:#FFF }
.col { display:flex; flex-direction:column }
.gap-10 { gap:0.625rem }
.h-80 { flex-shrink:0; height:5.0rem }
.pad-10 { padding:0.625rem }
.tt { transition-duration:0.5s; transition-property:height }
.hover\:bg-primarylight:hover { background-color:#3281cf }</style>

<div class="col gap-10 tt h-80">
  <div>Hello</div>
  <button class="bg-primary clr-white pad-10 hover:bg-primarylight" data-on-click="Expand" data-target="Contents">Expand</button>
</div>
</div>
  </div>
</div>


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

