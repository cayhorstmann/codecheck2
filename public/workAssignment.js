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
  
  function adjustDocHeight(iframe, request) {
    console.log({frame: iframeKey.get(iframe), oldHeight: iframe.scrollHeight, newHeight: request.param.docHeight })
    const newHeight = request.param.docHeight;
    if (iframe.scrollHeight < newHeight)
      iframe.style.height = newHeight + 'px'
  }

  function restoreStateOfProblem(iframe, request) {
    let key = iframeKey.get(iframe) 
    if (key in work.problems) {
      iframe.contentWindow.postMessage({ request, param: work.problems[key].state }, '*');
    } else {
      iframe.contentWindow.postMessage({ request, param: null }, '*')
    }
    updateScoreDisplay();     
  }

  async function sendScoreAndState(iframe, request) {    
    if (!assignment.isStudent) return // Viewing as instructor
    let key = iframeKey.get(iframe)  
    // Don't want qid which is also in request.param
    work.problems[key] = { score: request.param.score, state: request.param.state }
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
    savedCopyCheckbox.checked = true 
    if (useTitles) {
      select.disabled = false      
      buttonDiv.children[1].classList.remove('hc-disabled')
    } else {
      for (const b of buttonDiv.children)
        b.classList.remove('hc-disabled')
    }
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
    } else {
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
    } else {
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
      adjustDocHeight(iframe, event.data)
    else if (event.data.query === 'retrieve') 
      restoreStateOfProblem(iframe, event.data)      
    else if (event.data.query === 'send') 
      sendScoreAndState(iframe, event.data)
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
  
  if (assignment.isStudent) {
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
})
