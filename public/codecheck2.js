function inIframe() {
  try {
    return window.self !== window.top
  } catch (e) {
    return true
  }	
}

let horstmann_config = {
  score_change_listener: (element, state, score) => {
    if (inIframe()) {      
      const param = { state, score }
      const data = { query: 'send', param }
      window.parent.postMessage(data, '*' )
    }
  },
  retrieve_state: (element, callback) => {
    if (inIframe()) {
      if ('restoredStudentWork' in horstmann_config)
        callback(null, horstmann_config.restoredStudentWork)
      else
        horstmann_config.restoreStudentWork = callback
    }
    else
      callback(null, null)
  },
}

// gettext
let _ = x => x

document.addEventListener('DOMContentLoaded', function () {
  if (inIframe()) {
    function receiveMessage(event) {
      if (event.data.request) { // It's a response
        const request = event.data.request    
        if (request.query === 'retrieve') { // LTIHub v2
          if ('restoreStudentWork' in horstmann_config)
            horstmann_config.restoreStudentWork(null, event.data.param)
          else
            horstmann_config.restoredStudentWork = event.data.param
        }
      }
    }

    window.addEventListener('message', receiveMessage, false);

    let docHeight = 0  
    function sendDocHeight() {
      window.scrollTo(0, 0)
      const SEND_DOCHEIGHT_DELAY = 100
      setTimeout(() => { 
        let newDocHeight = document.documentElement.scrollHeight + document.documentElement.offsetTop
        if (docHeight != newDocHeight) {
          docHeight = newDocHeight
          const data = { query: 'docHeight', param: { docHeight } }
          window.parent.postMessage(data, '*' )
        } 
      }, SEND_DOCHEIGHT_DELAY)
    }  

    document.body.style.height = '100%'
    document.body.style.overflow = 'hidden'
    // ResizeObserver did not work          
    const mutationObserver = new MutationObserver(sendDocHeight);
    mutationObserver.observe(document.documentElement, { childList: true, subtree: true })    
    
    const data = { query: 'retrieve', param: { } }  
    console.log('Posting to parent', data)
    window.parent.postMessage(data, '*' )
  } else {
    horstmann_config.download = window.download // set in download.js
  }
})
