window.addEventListener('DOMContentLoaded', () => {
  const responseDiv = document.getElementById('response')
  const savedCopyCheckbox = document.querySelector('#savedcopy > input')
  const iframeKey = new Map()

  function problemKey(problem) {
    return problem.qid !== undefined ? problem.qid : problem.URL
  }
  
  function isProblemKeyFor(key, problem) {
      // Textbook repo
      if ('qid' in problem) return problem.qid === key
      // Some legacy CodeCheck problems have butchered keys such as 0101407088y6iesgt3rs6k7h0w45haxajn 
      else return problem.URL.endsWith(key)
  }
  
  function sendingIframe(event) {
    for (const f of document.getElementsByClassName('exercise-iframe'))
      if (f.contentWindow === event.source) return f
    return undefined
  }

  function updateScoreInElementText(e, score, title) {
    const text = e.textContent
    let match = text.match(/ \(|✓/u)
    let index = match === null ? text.length : match.index
    const label = score >= 0.995 ? '✓' : ' (' + percent(score) + ')'
    e.textContent = text.substring(0, index) + label
    if (title !== undefined && title !== '') e.title = title
  }

  function updateScoreDisplay() {
    if (studentID === '') return // Instructor viewing for cloning
    let result = 0
    let sum = 0
    let explanation = ''
    for (let i = 0; i < assignment.problems.length; i++) {
      const p = assignment.problems[i]
      if (p.weight > 0) {
        sum += p.weight
        // really want let score = work.problems[problemKey(p)].score
        for (const key in work.problems) {
          if (isProblemKeyFor(key, p)) { 
            let score = work.problems[key].score
            updateScoreInProblemSelector(i, score)
            result += p.weight * score
            if (explanation !== '') explanation += ' + '
            explanation += percent(score)
            if (p.weight !== 1) explanation += '×' + p.weight.toFixed(2).replace(/\.?0+$/, '')
          }
        }
      }          
    }
    if (explanation !== '' && sum !== 1) 
      explanation = '(' + explanation + ') / ' + sum.toFixed(2).replace(/\.?0+$/, '')
    
    result = sum === 0 ? sum : result / sum
    updateScoreInElementText(document.getElementById('heading'), result, explanation)    
  }
  
  function adjustDocHeight(iframe, newHeight) {
    // console.log({frame: iframeKey.get(iframe), oldHeight: iframe.scrollHeight, newHeight })
    if ('chrome' in window) { // https://stackoverflow.com/questions/4565112/javascript-how-to-find-out-if-the-user-browser-is-chrome
      const CHROME_FUDGE = 32 // to prevent scroll bars in Chrome
      newHeight += CHROME_FUDGE
	}
    if (iframe.scrollHeight < newHeight)
      iframe.style.height = newHeight + 'px'
  }

  function getStateOfProblem(iframe) {
    let key = iframeKey.get(iframe) 
    updateScoreDisplay(); // TODO: Why?         
    if (key in work.problems) return work.problems[key].state
    else return null // TODO: Why not undefined
  }

  async function sendScoreAndState(iframe, score, state) {    
    if (!assignment.isStudent) return // Viewing as instructor
    let key = iframeKey.get(iframe)  
    // Don't want qid which is also in request.param
    work.problems[key] = { score, state }
    updateScoreDisplay();     
    try {
      responseDiv.textContent = ''
      work.submittedAt = new Date(Date.now() - assignment.receivedAt + Date.parse(assignment.sentAt)).toISOString()
      if (lti !== undefined) {
        lti.work = work
        await postData("/lti/saveWork", lti)
      } else {
        await postData("/saveWork", work)
      }
    } catch (e) {
      responseDiv.textContent = e.message 
    }  
  }
  
  // Problem selection
  
  let useTitles = false
  let buttonDiv = undefined
  let select = undefined
  
  function initializeProblemSelectorUI() {
	if (assignment.problems.length === 1) {
	  document.getElementById('abovebuttons').style.display = 'none'
	  return
    } 
		
    buttonDiv = document.createElement('div')
    buttonDiv.id = 'buttons'
    document.body.appendChild(buttonDiv)      
    useTitles = assignment.problems.some(p => 'title' in p)
    if (useTitles) {
      document.getElementById('abovebuttons').textContent = 'Use the selector below to view all parts of the assignment.'
      select = document.createElement('select')
      select.disabled = true
      select.addEventListener('change', () => selectProblem(select.selectedIndex))
      buttonDiv.appendChild(select)
      buttonDiv.appendChild(createButton('hc-step hc-disabled', 'Next', 
        () => selectProblem(select.selectedIndex + 1)))   
    }     
  }
  
  function addProblemSelector(index, title) {
	if (assignment.problems.length === 1) return  
    const number = '' + (index + 1)
    if (useTitles) {
      const option = document.createElement('option')
      option.textContent = number + '. ' + (title === undefined ? '' : title)
      select.appendChild(option)
    } else {
      buttonDiv.appendChild(createButton('hc-step hc-disabled', number, () => selectProblem(index)))      
    }    
  }
    
  function activateProblemSelection() {
	if (useTitles) {
	  select.disabled = false      
	  buttonDiv.children[1].classList.remove('hc-disabled')
	} else if (assignment.problems.length > 1) {
	  for (const b of buttonDiv.children)
	    b.classList.remove('hc-disabled')
	} 
    savedCopyCheckbox.checked = true 
    const tab = 'tab' in work ? work.tab : 0
    setTimeout(() => selectProblem(tab), 1000)
  }
  
  function selectProblem(tab) {
    const iframes = document.getElementsByClassName('exercise-iframe')
    
    let index = parseInt(tab) // Legacy--used to be integers
    if (isNaN(index)) {
      for (let i = 0; isNaN(index) && i < iframes.length; i++)
        if (iframeKey.get(iframes[i]) === tab)
          index = i
      if (isNaN(index)) index = 0            
    }
    if (!savedCopyCheckbox.checked) return
    if (index < 0 || index >= assignment.problems.length) return
    if (useTitles) {
      select.selectedIndex = index      
    } else if (assignment.problems.length > 1) {
      for (let i = 0; i < buttonDiv.children.length; i++)
        if (i === index)
          buttonDiv.children[i].classList.add('active')
        else
          buttonDiv.children[i].classList.remove('active')      
    }
    for (let i = 0; i < iframes.length; i++) {
      const iframe = iframes[i]
      if (i === index) {    
        if (!iframe.src) iframe.src = assignment.problems[i].URL
        iframe.style.display = 'block'
        work.tab = iframeKey.get(iframe)
      } else { 
        iframe.style.display = 'none'
      }
    }
  }
  
  function updateScoreInProblemSelector(index, score) {
    if (useTitles) {
      updateScoreInElementText(select.children[index], score)            
    } else if (assignment.problems.length > 1) {
      updateScoreInElementText(buttonDiv.children[index], score)
    }
  }
  
  // Start of initialization
  
  assignment.receivedAt = Date.now()
  
  if (lti !== undefined) 
    window.history.replaceState(null, '', '/')  
  
  for (const e of document.getElementsByClassName('ccid')) 
    if (studentID !== '') 
      e.textContent = studentID
    else
      e.parentNode.style.display = 'none'
  
  window.addEventListener("message", event => {
    let iframe = sendingIframe(event)
    if (event.data.query === 'docHeight') 
      adjustDocHeight(iframe, event.data.param.docHeight)
    else if (event.data.query === 'retrieve') {
      const state = getStateOfProblem(iframe)      
      iframe.contentWindow.postMessage({ request: event.data, param: state }, '*')
    }
    else if (event.data.query === 'send') 
      sendScoreAndState(iframe, event.data.param.score, event.data.param.state)
    else if (event.data.subject === 'lti.frameResize')
      adjustDocHeight(iframe, event.data.height)
    else if (event.data.subject === 'SPLICE.reportScoreAndState')
      sendScoreAndState(iframe, event.data.score, event.data.state)
    else if (event.data.subject === 'SPLICE.getState') {
      const state = getStateOfProblem(iframe)
      iframe.contentWindow.postMessage({ subject: 'SPLICE.getState.response', message_id: event.data.message_id, state }, '*')      
    }    
  }, false);
  
  /*
  document.getElementById('switchID').appendChild(createButton('hc-command', 'Switch to this ID', () => {
      responseDiv.textContent = ''
      const newccid =  document.getElementById('newccid').value
      if (/^(([aeiouy][bcdfghjklmnpqrstvwxz]){2}|([bcdfghjklmnpqrstvwxz][aeiouy]){2})(-(([aeiouy][bcdfghjklmnpqrstvwxz]){2}|([bcdfghjklmnpqrstvwxz][aeiouy]){2})){3}$/.test(newccid)) {
        location = `${location.href.split(/[?#]/)[0]}?newid=${newccid}`                
      } else {
        responseDiv.textContent = 'Not a valid CodeCheck ID'
      }        
    }))
  */
  if ('clearIDURL' in assignment)
    document.getElementById('clearID').appendChild(createButton('hc-command', 'Clear ID now', () => {
      location = assignment.clearIDURL    
    }))
      
  initializeProblemSelectorUI()      
  for (let i = 0; i < assignment.problems.length; i++) {
    const iframe = document.createElement('iframe')    
    iframeKey.set(iframe, problemKey(assignment.problems[i]))
    iframe.className = 'exercise-iframe'
    document.body.appendChild(iframe)
    iframe.style.display = 'none'
    addProblemSelector(i, assignment.problems[i].title)
  }
  
  if (assignment.noHeader) {
	  document.getElementsByTagName('details')[0].style.display = 'none'
  }

  if (assignment.isStudent) {
    if('deadline' in assignment){
      document.getElementById('deadline').textContent = "Deadline:"
      let deadline = assignment.deadline
      let deadlineUTC = new Date(deadline).toUTCString()
      let deadlineLocal = new Date(Date.parse(deadline))
      document.getElementById('deadlineLocal').textContent = deadlineLocal
      document.getElementById('deadlineUTC').textContent = deadlineUTC + " (UTC)"
    }
    if (lti === undefined) {
      document.getElementById('studentLTIInstructions').style.display = 'none'
      const returnToWorkURLSpan = document.getElementById('returnToWorkURL') 
      returnToWorkURLSpan.textContent = assignment.returnToWorkURL
    
      document.getElementById('returnToWork').appendChild(createButton('hc-command', 'Copy', () => { 
        window.getSelection().selectAllChildren(returnToWorkURLSpan) 
        document.execCommand('copy')
        window.getSelection().removeAllRanges() 
      }))
      
      if (assignment.editKeySaved) {
        activateProblemSelection()
        document.getElementById('savedcopy').style.display = 'none'
      } else {
        savedCopyCheckbox.checked = false
        savedCopyCheckbox.addEventListener('change', () => {    
          if (savedCopyCheckbox.checked) {
            activateProblemSelection()
          } else
            savedCopyCheckbox.checked = true // Can't uncheck                  
        }) 
      }      
    } else {
      document.getElementById('submitLTIButton').appendChild(createButton('hc-command', 'Resend Score', async () => {
        try {
          responseDiv.textContent = ''
          let response = await postData("/lti/sendScore", lti) // TODO: URLs from server
          responseDiv.textContent = response.outcome === 'success' ? `Score of ${percent(response.score)} recorded` : response.outcome 
        } catch (e) {
          responseDiv.textContent = e.message 
        }  
      }))  
      document.getElementById('studentInstructions').style.display = 'none'
      activateProblemSelection()
    }       
    document.getElementById('instructorInstructions').style.display = 'none'
  } else { // Instructor view                
    if ('editAssignmentURL' in assignment)
      document.getElementById('viewingAsInstructor').appendChild(createButton('hc-command', 'Edit', () => {
        window.location.href = assignment.editAssignmentURL        
      }))    
    if ('cloneURL' in assignment) 
      document.getElementById('viewingAsInstructor').appendChild(createButton('hc-command', 'Clone', () => {
        window.open(assignment.cloneURL, '_blank')        
      }))
    if ('viewSubmissionsURL' in assignment)
      document.getElementById('viewingAsInstructor').appendChild(createButton('hc-command', 'View submissions', () => {
        window.open(assignment.viewSubmissionsURL, '_blank')        
      }))    
        
    const privateURLSpan = document.getElementById('privateURL')
    if ('privateURL' in assignment) {
      privateURLSpan.parentNode.appendChild(createButton('hc-command', 'Copy', () => { 
        window.getSelection().selectAllChildren(privateURLSpan); 
        document.execCommand('copy');
        window.getSelection().removeAllRanges(); }))
      document.getElementById('publicURL').textContent = assignment.publicURL
      privateURLSpan.textContent = assignment.privateURL
      privateURLSpan.parentNode.parentNode.style.display = 'block'
    }
    else
      privateURLSpan.parentNode.parentNode.style.display = 'none'
        
    activateProblemSelection()
    document.getElementById('studentInstructions').style.display = 'none'
    document.getElementById('studentLTIInstructions').style.display = 'none'
  }
  
  if (assignment.isStudent) {
    if (assignment?.comment !== "") 
      document.getElementById('student_comment').textContent = assignment.comment
    else
      document.getElementById('student_comment_div').style.display = 'none'
  }
  else if (assignment.saveCommentURL) {
    document.getElementById('instructor_comment').textContent = assignment?.comment
    const submitButton = createButton('hc-command', 'Save Comment', async () => {
      let request = {
          assignmentID: assignment.assignmentID,
          workID: assignment.workID, // undefined when isStudent
          comment: document.getElementById('comment').value,
      }
      
      submitButton.disabled = true
      responseDiv.style.display = 'none'
      try {
        await postData(assignment.saveCommentURL, request)
      } catch (e) {
        responseDiv.textContent = e.message           
        responseDiv.style.display = 'block'
      }
      submitButton.disabled = false    
    })
    document.getElementById('instructor_comment_div').appendChild(submitButton) 
  } 
  else {
    document.getElementById('instructor_comment_div').style.display = 'none'
  }

})
