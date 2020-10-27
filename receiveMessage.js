/*

  Protocol:

data.
  query      (request from parent to child) 'docHeight', 'getContent', 'restoreState' (LTIHub v1)                                 
             (request from child to parent) 'docHeight', 'send', 'retrieve' (LTIHub v2)
  request    (response) the request

  
  state      (restoreState request from parent to child, v1)
  state      (getContent response from child to parent, v1) 
  
  nonce      (request from child to parent) a nonce to be returned with
  the response, for non-void requests ('retrieve', v2)

  docHeight  (docHeight request, response from child to parent, v1) 

  score      (getContent response from child to parent, v1) 

  param      (request or response) parameter object (v2)


v2 details:

Always from child to parent

{ query: 'docHeight', param: { docHeight: ... } }
  No qid because applies to whole frame
  No response
 
{ query: 'send', param: { qid: ..., state: ..., score: ... }}
  Score between 0 and 1
  No response

{ query: 'retrieve', param: { qid: ... } }
  Response: { request: ..., param: state }

TODO: Why not param: { state: ..., score: ... }
  If we want that, must adapt workAssignment.js, receiveMessage.js, codecheck.js

*/

if (window.self !== window.top) { // iframe
  if (!('EPUB' in window))
    window.EPUB = {}
  if (!('Education' in window.EPUB)) {
    window.EPUB.Education = {
      nonceMap: {},
      retrieveCallback: undefined, // LTIHub v1
      retrieve: (request, callback) => {
        window.EPUB.Education.retrieveCallback = callback // LTIHub v1
        if ('stateToRestore' in window.EPUB.Education) { // LTIHub v1, restore data already arrived
          callback({ data: [ { data: window.EPUB.Education.stateToRestore } ] })
          delete window.EPUB.Education.stateToRestore
          return
        }
        // Register callback
        const nonce = generateUUID()
        window.EPUB.Education.nonceMap[nonce] = callback
        if (window.EPUB.Education.version !== 1) { // LTIHub v1
          // Pass request and nonce to parent
          // TODO: VitalSource format
          const qid = request.filters[0].activityIds[0]
          const param = { qid }
          const data = { query: 'retrieve', param, nonce }
          window.parent.postMessage(data, '*' )
        }
        const MESSAGE_TIMEOUT = 5000
        setTimeout(() => {
          if ('stateToRestore' in window.EPUB.Education) { // LTIHub v1, restore data already arrived and delivered
            delete window.EPUB.Education.stateToRestore
            return              
          }            

          if (!(nonce in window.EPUB.Education.nonceMap)) return
          delete window.EPUB.Education.nonceMap[nonce]
          // TODO: VitalSource format
          callback({ data: [ { data: null } ] })
        }, MESSAGE_TIMEOUT)
      },
      send: (request, callback) => {
        if (window.EPUB.Education.version === 1) return // LTIHub v1
        // TODO: VitalSource format
        const param = { state: request.data[0].state.data, score: request.data[0].results[0].score, qid: request.data[0].activityId }
        const data = { query: 'send', param }
        window.parent.postMessage(data, '*' )
      },
    }
  }

  // https://stackoverflow.com/questions/105034/create-guid-uuid-in-javascript

  function generateUUID() { // Public Domain/MIT
    var d = new Date().getTime() 
    var d2 = (performance && performance.now && (performance.now() * 1000)) || 0 // Time in microseconds since page-load or 0 if unsupported
    return 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, c => {
      let r = Math.random() * 16 // random number between 0 and 16
      if(d > 0) { // Use timestamp until depleted
        r = (d + r) % 16 | 0
        d = Math.floor(d / 16)
      } else { // Use microseconds since page-load if supported
        r = (d2 + r) % 16 | 0
        d2 = Math.floor(d2/16)
      }
      return (c === 'x' ? r : (r & 0x3 | 0x8)).toString(16)
    })
  }
  
  let element = undefined
  
  window.addEventListener('load', event => {
    const interactiveElements = [...document.querySelectorAll('div, ol')].
          filter(e => {
            const ty = e.tagName
            const cl = e.getAttribute('class')
            return cl && (ty === 'div' && cl.indexOf('horstmann_') == 0 || ty === 'ol' && (cl.indexOf('multiple-choice') == 0 || cl.indexOf('horstmann_ma') == 0))
          })    
    element = interactiveElements[0]

    document.body.style.overflow = 'hidden'         
    const resizeObserver = new ResizeObserver(entries => {
      if (window.EPUB.Education.version !== 1) { // TODO
        const FUDGE = 50
        const docHeight = document.body.children[0].scrollHeight + FUDGE
        const data = { query: 'docHeight', param: { docHeight } }
        window.parent.postMessage(data, '*' )
      }
    })
    /* 
       Weirdly, when listening to document.body or 
       document.documentElement, the document height keeps
       getting increased
    */    
    resizeObserver.observe(document.body.children[0])
  })

  window.addEventListener("message", event => {    
    if ('request' in event.data) { // It's a response
      const request = event.data.request    
      if (request.query === 'retrieve') { // LTIHub v2        
        const state = event.data.param
        // TODO Old VitalSource API
        // let state = response.data[0].data
        const arg = { data: [ { data: state } ] }
        if (request.nonce in window.EPUB.Education.nonceMap) {
          // If not, already timed out
          window.EPUB.Education.nonceMap[request.nonce](arg)
          delete window.EPUB.Education.nonceMap[request.nonce]
        }
      }
      // Handle other responses  
    } else { // It's a request   
      if (event.data.query === 'docHeight') { // LTIHub v1
        const docHeight = document.body.children[0].scrollHeight
        document.documentElement.style.height = docHeight + 'px'
        document.body.style.height = docHeight + 'px'
        document.body.style.overflow = 'auto' 
        let response = { request: event.data, docHeight }
        event.source.postMessage(response, '*' )
        window.EPUB.Education.version = 1
      }
      else if (event.data.query === 'getContent') { // LTIHub v1
        const docHeight = document.body.children[0].scrollHeight
        document.documentElement.style.height = docHeight + 'px'
        document.body.style.height = docHeight + 'px'
        const id = element.closest('li').id
        const score = { correct: Math.min(element.correct, element.maxscore), errors: element.errors, maxscore: element.maxscore, activity: id }
        let response = { request: event.data, score: score, state: element.state }
        event.source.postMessage(response, '*' )          
      } else if (event.data.query === 'restoreState') { // LTIHub v1
        window.EPUB.Education.stateToRestore = event.data.state 
        /*
          It is possible that the element already made a
          retrieve request (which goes unanswered by the parent). 
        */
        if (window.EPUB.Education.retrieveCallback !== undefined) { // retrieve request already made
          window.EPUB.Education.retrieve(undefined, window.EPUB.Education.retrieveCallback)
          // delete window.EPUB.Education.retrieveCallback
          // Not deleting--in the instructor view assignments, called more than once
        }                  
      }
    }
  }, false)
}
