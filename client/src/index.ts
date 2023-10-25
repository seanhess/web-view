import { render, patch, create } from "omdomdom/lib/omdomdom.es.js"




// import { listenEvents } from './events';
// import { WEBSOCKET_ADDRESS, Messages } from './Messages'
// import { INIT_PAGE, INIT_STATE, State, Class } from './types';
// import { fromVDOM, VDOM } from './vdom'
import  { listenChange, listenClick, listenFormSubmit } from './events'


// const CONTENT_ID = "yeti-root-content"

// console.log("VERSION 2", INIT_PAGE, INIT_STATE)
console.log("Hyperbole 0.1.3")

// const protocol = window.location.protocol === 'https:' ? 'wss:' : 'ws:';
// const address = `${protocol}//${window.location.host}`
// const socket = new WebSocket(address)




listenClick(async function(target:HTMLElement, action:string) {
  console.log("CLICK", target.id, action)
  runAction(target, action)
})

listenFormSubmit(async function(target:HTMLElement, action:string, form:FormData) {
  console.log("FORM", target.id, action,form)
  runAction(target, action, form)
})

listenChange(async function(target:HTMLElement, action:string) {
  console.log("CHANGE", target.id, action)
  runAction(target, action)
})

async function sendAction(id:string, action:string, form?:FormData) {
  let url = new URL(window.location.href)
  url.searchParams.append("id", id)
  url.searchParams.append("action", action)

  console.log("ACTION", url.toString())

  let res = await fetch(url, {
    method: "POST",
    headers: { 'Accept': 'text/html', 'Content-Type': 'application/x-www-form-urlencoded'},
    body: toSearch(form)
  })

  return res.text()
}

async function runAction(target:HTMLElement, action:string, form?:FormData) {
  target.classList.add("request")
  let ret = await sendAction(target.id, action, form)

  // Patch the local target instead of replacing
  const wrapper = document.createElement("div")
  wrapper.setAttribute("id", target.id)
  wrapper.setAttribute("class", target.getAttribute("class"))
  wrapper.innerHTML = ret

  const next = create(wrapper)
  patch(next, create(target))

  target.classList.remove("request")
}

function toSearch(form?:FormData):URLSearchParams | undefined {
  if (!form) return undefined
    
  const params = new URLSearchParams()


  form.forEach((value, key) => {
    params.append(key, value as string)
  })

  return params
}




// function init() {
//   let root = document.body
//
//   let nodes:VNode[] = fromStringToVNode(root.innerHTML) as VNode[]
//   let newRoot = m("body", {}, nodes)
//   // console.log("PATCH", root, newRoot)
//   patch(root, newRoot)
// }

// document.addEventListener("DOMContentLoaded", () => {
//   // init()
// })

// socket.addEventListener('open', (event) => {
//   console.log("Opened")
// })
//
// socket.addEventListener('error', (event) => {
//   console.log("Error")
// })
//
// socket.addEventListener('message', (event) => {
//   console.log("message", event.data)
//   let {command, data} = parseCommand(event.data)
//   console.log("CMD", command)
//   console.log("DATA", data)
// })
//
// socket.addEventListener('close', (event) => {
//   console.log("close")
// })
//
// type Command<T> = {
//   command: string,
//   data: T
// }
//
// function parseCommand<T>(message:string): Command<T> {
//   const match = message.match(/^(\w+)\s+(.*)$/)
//
//   if (!match) console.error("Could not parse command: ", message)
//
//   return {
//     command: match[1],
//     data: JSON.parse(match[2])
//   }
// }

