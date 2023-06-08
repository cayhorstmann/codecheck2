// This file is included by the activities on www.interactivities.ws

// The following starts out with splice-iframe.js, and then translates VitalSource EPUB to SPLICE 

window.addEventListener('load', event => {

  const scores = {}
  const states = {}
  let callbacks = undefined
  let docHeight = 0
  
  const sendDocHeight = () => {
    if (window.self === window.top) return // not iframe

    window.scrollTo(0, 0)
    const SEND_DOCHEIGHT_DELAY = 100
    setTimeout(() => {
      let newDocHeight = document.body.scrollHeight + document.body.offsetTop
      if (docHeight != newDocHeight) {
        docHeight = newDocHeight
        const data = { subject: 'lti.frameResize', message_id: generateUUID(), height: docHeight }
        window.parent.postMessage(data, '*' )
        if (window.SPLICE.logging) console.log('postMessage to parent', data)
      } 
    }, SEND_DOCHEIGHT_DELAY)
  }

  // https://stackoverflow.com/questions/105034/create-guid-uuid-in-javascript
  const generateUUID = () => { // Public Domain/MIT
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

  if (!('SPLICE' in window)) {
    window.SPLICE = {
      logging: true,
      getScore: (location, callback) => {
        if (callbacks === undefined) {
          callback(states[location])
        } else {
          callbacks.push({location, callback})
        }
      },
      reportScoreAndState: (location, score, state) => {
        scores[location] = score
        states[location] = state
        if (window.self === window.top) return // not iframe
        let averageScore = 0
        let n = 0
        for (const location in scores) {
          averageScore += scores[location];
          n++
        }
        if (n > 0) averageScore /= n
        const message = { subject: 'SPLICE.reportScoreAndState', message_id: generateUUID(), score: averageScore, state: states }
        window.parent.postMessage(message, '*' )
        if (window.SPLICE.logging) console.log('postMessage to parent', message)          
      },
      sendEvent: (location, name, data) => {
	    const message = { subject: 'SPLICE.sendEvent', message_id: generateUUID(), name, data } 	  
        window.parent.postMessage(message, '*' )
        if (window.SPLICE.logging) console.log('postMessage to parent', message)          
	  }
    }
  }

  if (window.self !== window.top) { // iframe
    const message = { subject: 'SPLICE.getState', message_id: generateUUID() }
    window.parent.postMessage(message, '*' )
    if (window.SPLICE.logging) console.log('postMessage to parent', message)
    callbacks = []
    const MESSAGE_TIMEOUT = 5000
    setTimeout(() => {
      if (callbacks === undefined) return
      for (const { location, callback } of callbacks) {
        scores[location] = 0
        states[location] = undefined
        callback(undefined, { code: 'timeout' })
      }
      callbacks = undefined
    }, MESSAGE_TIMEOUT)
  }
  
  window.addEventListener('message', event => {
    if (!(event.data instanceof Object)) return
    if (!(event.data.subject instanceof String && event.data.subject.startsWith('SPLICE.'))) return
    if (event.data.subject.endsWith('response')) {
      if (window.SPLICE.logging) console.log('postmessage response', event)
      if (event.data.subject === 'SPLICE.getState.response') {
        if (callbacks === undefined) return // Already timed out
        for (const location in event.data.state) {
          scores[location] = 0
          states[location] = event.data.state[location]
        }
          
        for (const { location, callback } of callbacks) {
          callback(states[location], event.data.error)
        }
        callbacks = undefined
      }
      // Handle other responses         
    }
    // TODO: SPLICE messages from children
  }, false)

  sendDocHeight()
  document.body.style.overflow = 'hidden'
  // ResizeObserver did not work
  const mutationObserver = new MutationObserver(sendDocHeight);
  mutationObserver.observe(document.body, { childList: true, subtree: true })
  
  // Translating EPUB to SPLICE
  if (!('EPUB' in window))
    window.EPUB = {}
  if (!('Education' in window.EPUB)) {
    window.EPUB.Education = {
      retrieve: (request, callback) => {
		const location = request.filters[0].activityIds[0]
		SPLICE.getState(location, (location, state) => {
			callback({ data: [ { data: state } ] })
		})
      },
      send: (request, callback) => {
        const location = request.data[0].activityId
        const score = request.data[0].results[0].score
        const state = request.data[0].state.data
        SPLICE.reportScoreAndState(location, score, state)
      },
    }
  }
    
})

