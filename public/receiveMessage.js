/*

  Shim translating the VitalSource protocol to parent frame messaging.
  Was meant for codecheck, but now we use codecheck2.js
  TODO Isn't actually used with interactives--they still use the old version
  TODO Modernize the old one and place here. No LTI, just parent messaging
  TODO Or use codecheck2, so that horstmann_config's functions are overwritten, wiping out VitalSource
  
  Protocol to parent frame:

data.
  query      (request from child to parent) 'docHeight', 'send'
  request    (response) the request
  
{ query: 'docHeight', param: { docHeight: ... } }
 
{ query: 'send', param: { score: ... }}
  Score between 0 and 1

*/

// TODO Avoid duplication
async function postData(url = '', data = {}) {
  const response = await fetch(url, {
    method: 'POST', // *GET, POST, PUT, DELETE, etc.
    mode: 'cors', // no-cors, *cors, same-origin
    cache: 'no-cache', // *default, no-cache, reload, force-cache, only-if-cached
    credentials: 'include', // include, *same-origin, omit
    headers: {
      'Content-Type': 'application/json'
    },
    redirect: 'follow', // manual, *follow, error
    referrerPolicy: 'no-referrer', // no-referrer, *no-referrer-when-downgrade, origin, origin-when-cross-origin, same-origin, strict-origin, strict-origin-when-cross-origin, unsafe-url
    body: JSON.stringify(data) // body data type must match "Content-Type" header
  });
  if (response.ok)
    return await response.json() // parses JSON response into native JavaScript objects
  else {
    const body = await response.text()
    if (response.status === 500) console.log(body)
    const msg = 
      response.status === 500 ? 'Server error' : 
      response.status === 400 ? `Error: ${body}` : // Bad reqest
        `Error ${response.status} ${response.statusText}: ${body}`    
    throw new Error(msg)
  }
}

if (!('EPUB' in window))
  window.EPUB = {}
if (!('Education' in window.EPUB)) {
  window.EPUB.Education = {
    retrieve: async (request, callback) => {
      try {
        let response = await postData(lti.retrieveURL, {
          submissionID: lti.submissionID
        })
        // TODO: VitalSource format
        callback({ data: [ { data: response.state } ] })
      } catch {
        callback({ data: [ { data: null } ] })
      } 
    },
    send: (request, callback) => {
      const param = { state: request.data[0].state.data, score: request.data[0].results[0].score, qid: request.data[0].activityId }
      const data = { query: 'send', param }
      if (window.self !== window.top) { 
        window.parent.postMessage(data, '*' )
        sendDocHeight()
      }
      // TODO: Why??? 
      postData(lti.sendURL, { 
        state: param.state, 
        score: param.score,
        lis_outcome_service_url: lti.lis_outcome_service_url,
        lis_result_sourcedid: lti.lis_result_sourcedid,
        oauth_consumer_key: lti.oauth_consumer_key,
        submissionID: lti.submissionID           
      })
    },
  }
}

// https://stackoverflow.com/questions/105034/create-guid-uuid-in-javascript

let element = undefined
let docHeight = 0

function sendDocHeight() {
  if (window.self === window.top) return 
  window.scrollTo(0, 0)
  const SEND_DOCHEIGHT_DELAY = 100
  setTimeout(() => { 
    const container = element === undefined ? document.body : element.closest('li').parentNode
      // When using container = document.documentElement, the document grows without bound
      // TODO: Why does this work in codecheck.js but not here?
    let newDocHeight = container.scrollHeight + container.offsetTop
    if (docHeight != newDocHeight) {
      docHeight = newDocHeight
      const data = { query: 'docHeight', param: { docHeight } }
      window.parent.postMessage(data, '*' )
    } 
  }, SEND_DOCHEIGHT_DELAY)
}

if (window.self !== window.top) {
  window.addEventListener('load', event => {
    const interactiveElements = [...document.querySelectorAll('div, ol')].
          filter(e => {
            const ty = e.tagName
            const cl = e.getAttribute('class')
            return cl && (ty === 'div' && cl.indexOf('horstmann_') == 0 || ty === 'ol' && (cl.indexOf('multiple-choice') == 0 || cl.indexOf('horstmann_ma') == 0))
          })
      element = interactiveElements[0]
      sendDocHeight()
      // document.body.style.overflow = 'hidden'
      // ResizeObserver did not work         
      const mutationObserver = new MutationObserver(sendDocHeight);
    mutationObserver.observe(element === undefined ? document.body : element, { childList: true, subtree: true })    
  })
} else if (lti !== undefined) 
  window.history.replaceState(null, '', '/')



