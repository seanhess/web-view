// import { hydrate, patch, render, DOMNode, m, VNode, Flags, style } from 'million';
// import { fromDomNodeToVNode, fromStringToDomNode } from 'million/utils';
// import { listenEvents } from './events';
// import { WEBSOCKET_ADDRESS, Messages } from './Messages'
// import { INIT_PAGE, INIT_STATE, State, Class } from './types';
// import { fromVDOM, VDOM } from './vdom'
import  { listenClick, listenFormSubmit } from './events'


// const CONTENT_ID = "yeti-root-content"

// console.log("VERSION 2", INIT_PAGE, INIT_STATE)
console.log("TEST INTERFACE?")

// const protocol = window.location.protocol === 'https:' ? 'wss:' : 'ws:';
// const address = `${protocol}//${window.location.host}`
// const socket = new WebSocket(address)


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

listenClick(async function(target:HTMLElement, action:string) {
  // console.log("CLICK!", target.id, action)

  let ret = await sendAction(target.id, action)
  target.outerHTML = ret
})

listenFormSubmit(async function(target:HTMLElement, action:string, form:FormData) {
  console.log("FORM", target.id, action,form)

  let ret = await sendAction(target.id, action, form)
  target.outerHTML = ret
})

function toSearch(form?:FormData):URLSearchParams | undefined {
  if (!form) return undefined
    
  const params = new URLSearchParams()

  form.forEach((value, key) => {
    params.append(key, value as string)
  })

  return params
}


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

