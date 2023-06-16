// Uses postData from util.js

window.horstmann_config = {
  inIframe: function () {
    try {
      return window.self !== window.top
    } catch (e) {
      return true
    } 
  },

  score_change_listener: (element, state, score) => {
    if ('lti' in horstmann_config) {
      postData(horstmann_config.lti.sendURL, { 
        state, 
        score,
        lis_outcome_service_url: horstmann_config.lti.lis_outcome_service_url,
        lis_result_sourcedid: horstmann_config.lti.lis_result_sourcedid,
        oauth_consumer_key: horstmann_config.lti.oauth_consumer_key,
        submissionID: horstmann_config.lti.submissionID           
      })      
    }
    else if (horstmann_config.inIframe()) {      
      const qid = horstmann_config.getInteractiveId(element) // TODO should be done by caller
      // const param = { state, score, qid }
      // const message = { query: 'send', param }
      const message = {
		subject: 'SPLICE.reportScoreAndState', 
        message_id: horstmann_config.generateUUID(),
        score,
        state 
	  }
      window.parent.postMessage(message, '*' )
    }
  },

   // https://stackoverflow.com/questions/105034/create-guid-uuid-in-javascript
  generateUUID: function() { // Public Domain/MIT
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
  },

  getInteractiveId: function(e) {
    while (e && e.parentNode && (!e.parentNode.className || e.parentNode.className.indexOf('interactivities') < 0))
      e = e.parentNode
    return e ? e.id : null
  },

  nonceMap: {},
  
  retrieve_state: async (element, callback) => {
    if ('lti' in horstmann_config) {
      try {
        let response = await postData(horstmann_config.lti.retrieveURL, {
          submissionID: horstmann_config.lti.submissionID
        })
        callback(element, response.state)
      } catch (e) {
        console.log(e)
        callback(element, null)
      }
    }
    else if (horstmann_config.inIframe()) {
      const message_id = horstmann_config.generateUUID()
      horstmann_config.nonceMap[message_id] = callback
      // const qid = horstmann_config.getInteractiveId(element) // TODO should be done by caller
      // const param = { qid } 
      // const message = { query: 'retrieve', param, nonce }
      const message = {
		subject: 'SPLICE.getState', 
        message_id
	  } 
      window.parent.postMessage(message, '*')
      const MESSAGE_TIMEOUT = 5000
      setTimeout(() => {
        if ((message_id in horstmann_config.nonceMap)) { 
          delete horstmann_config.nonceMap[message_id]
          callback(element, null)
        }
      }, MESSAGE_TIMEOUT)      
    }
    else
      callback(element, null)
  },
}

// TODO: Ugly hack for gettext
let _ = x => x

document.addEventListener('DOMContentLoaded', function () {
  if (horstmann_config.inIframe()) {
    function receiveMessage(event) {
      if (event.data.subject === 'SPLICE.getState.response') { 
        if (event.data.message_id in horstmann_config.nonceMap) {
          // If not, already timed out
          horstmann_config.nonceMap[event.data.message_id](null, event.data.state)
          delete horstmann_config.nonceMap[event.data.message_id]
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
          // docHeight = newDocHeight
          // const message = { query: 'docHeight', param: { docHeight } }
          const message = {
			subject: 'lti.frameResize', 
            message_id: horstmann_config.generateUUID(),
            height: newDocHeight,
            width: document.documentElement.scrollWidth + document.documentElement.offsetLeft 
		  }
          window.parent.postMessage(message, '*' )
        } 
      }, SEND_DOCHEIGHT_DELAY)
    }  

    // document.body.style.height = '100%'
    // document.body.style.overflow = 'hidden'
    // ResizeObserver did not work          
    const mutationObserver = new MutationObserver(sendDocHeight);
    mutationObserver.observe(document.documentElement, { childList: true, subtree: true })    
    
    const message = {
		subject: 'SPLICE.getState', 
        message_id: horstmann_config.generateUUID()
    } 
    window.parent.postMessage(message, '*' )
  } else {
    // TODO: Ugly?
    // set in download.js, used when initializing the UI
    horstmann_config.download = window.download
  }
})
