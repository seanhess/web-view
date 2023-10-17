// import { hydrate, patch, render, DOMNode, m, VNode, Flags, style } from 'million';
// import { fromDomNodeToVNode, fromStringToDomNode } from 'million/utils';
// import { listenEvents } from './events';
// import { WEBSOCKET_ADDRESS, Messages } from './Messages'
// import { INIT_PAGE, INIT_STATE, State, Class } from './types';
// import { fromVDOM, VDOM } from './vdom'
import  { listenClickAction, targetId } from './events'


// const CONTENT_ID = "yeti-root-content"

// console.log("VERSION 2", INIT_PAGE, INIT_STATE)
console.log("TEST INTERFACE?")

const protocol = window.location.protocol === 'https:' ? 'wss:' : 'ws:';
const address = `${protocol}//${window.location.host}`
const socket = new WebSocket(address)


async function sendAction(id:string, action:string) {
  let url = new URL(window.location.href)
  url.searchParams.append("id", id)
  url.searchParams.append("action", action)

  console.log("URL", url.toString())

  let res = await fetch(url, {
    method: "POST",
    headers: { 'Accept': 'text/html', 'Content-Type': 'application/x-www-form-urlencoded'},
    // body: form
  })

  return res.text()
}

listenClickAction(async function(target:HTMLElement, action:string) {
  console.log("CLICK!", targetId(target), action)
  let ret = await sendAction(targetId(target), action)
  console.log("RET", ret)

  target.outerHTML = ret
})


socket.addEventListener('open', (event) => {
  console.log("Opened")
})

socket.addEventListener('error', (event) => {
  console.log("Error")
})

socket.addEventListener('message', (event) => {
  console.log("message", event.data)
  let {command, data} = parseCommand(event.data)
  console.log("CMD", command)
  console.log("DATA", data)
})

socket.addEventListener('close', (event) => {
  console.log("close")
})

type Command<T> = {
  command: string,
  data: T
}

function parseCommand<T>(message:string): Command<T> {
  const match = message.match(/^(\w+)\s+(.*)$/)

  if (!match) console.error("Could not parse command: ", message)

  return {
    command: match[1],
    data: JSON.parse(match[2])
  }
}


// var currentState:State = INIT_STATE
// var rootElement:DOMNode
// var stylesheet:CSSStyleSheet
//
// const messages = new Messages()
// messages.onUpdate(update)
// messages.onClose(() => {
//   // reconnect on close
//   setTimeout(() => messages.connect(INIT_PAGE, currentState), 1000)
// })
// messages.connect(INIT_PAGE, currentState)
//
// listenEvents(messages)
//
// window.addEventListener("load", function() {
//   rootElement = document.getElementById("yeti-root-content")
//
//   let styleNode = document.getElementById("yeti-stylesheet") as any
//   stylesheet = styleNode.sheet
//
//   // hydreate the content
//   let initContent = fromDomNodeToVNode(rootElement)
//   console.log('INIT', initContent)
//   rootElement = patch(rootElement, initContent)
// })
//
// function update(newState:State, params:string, vdom:VDOM, classes:Class[]) {
//
//   // This is stripping tab characters in the data attributes, can't use tab as a delimiter
//   // let dom = fromStringToDomNode(html)
//   // let vnode = fromDomNodeToVNode(dom)
//   // rootElement = patch(rootElement, vnode)
//   let newRoot = m("div", {"id": CONTENT_ID}, fromVDOM(vdom))
//   rootElement = patch(rootElement, newRoot)
//
//   currentState = newState
//
//   // Update stylesheet
//   // let defs = classDefinitions(classes)
//
//   // hmm, only if it doesn't exist in the sheet
//   classes
//     .filter((c) => !hasRule(stylesheet.cssRules, c))
//     .forEach((c) => stylesheet.insertRule(c.cssText))
//
//   // wait, is this pushing in a circle?
//   updateHistory(newState, params, vdom)
//
// }
//
//
// function updateHistory(newState:State, params:string, vdom:VDOM) {
//   if (("?" + params) != location.search) {
//     // console.log("New History", params, location.search)
//     let url = location.origin + location.pathname + "?" + params
//     history.pushState([newState, params, vdom], "", url)
//   }
// }
//
// // History events
// window.addEventListener("popstate", function(e) {
//   let [newState, params, html, stylesheet] = e.state
//   update(newState, params, html, stylesheet)
// })
//
//
// // DYNAMIC STYLES ////////////////////
//
//
// // const styleSheet = new CSSStyleSheet();
// // styleSheet.replaceSync('body { background: red; }');
//
// // sheet.insertRule("* { color: blue; }");
//
// // Apply the stylesheet to a document
// // document.adoptedStyleSheets = [styleSheet];
//
//
// // function classDefinitions(classes:Classes):string[] {
// //   var defs = []
// //   for (var name in classes ) {
// //     defs.push(classDefinition(name, classes[name]))
// //   }
// //   return defs
// // }
//
//
//
// function hasRule(cssRules:CSSRuleList, cls:Class):boolean {
//   for (var i in cssRules) {
//     let rule = cssRules[i] as CSSStyleRule
//     if (rule.selectorText == cls.selectorText)
//       return true
//   }
//   return false
// }
