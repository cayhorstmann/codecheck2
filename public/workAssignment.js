window.addEventListener('DOMContentLoaded', () => {
  function hash(s) {
    let r = 0
    for (let i = 0; i < s.length; i++) r = (31 * r + s.charCodeAt(i)) % 4294967296 // 2 ** 32
    if (r >= 4294967296 / 2) r = r - 4294967296  
    return r      
  }
  
  function key(problem) {
    return problem.qid !== undefined ? problem.qid
      : problem.URL.substring(problem.URL.lastIndexOf("/") + 1).replace(/[^A-Za-z_-]/, '')
  }
  
  function score(problems, work) {
    let score = 0
    for (const problem of problems) {
      const workKey = key(problem)
      if (workKey in work) {
        score += work[workKey].score * problem.weight
      }
    }
    return score
  }

  const problems = assignmentData.assignment[hash(studentID) % assignmentData.assignment.length]  
  const responseDiv = document.getElementById('response')
  const savedCopyCheckbox = document.querySelector('#savedcopy > input')
  const buttonDiv = document.createElement('div')

  if (work === undefined) { 
    work = { ccid: studentID }
  } else {
    responseDiv.textContent = `Score: ${percent(score(problems, work))}`
    responseDiv.title = `Submitted at ${work.submittedAt}`
    responseDiv.style.display = 'block'
  }
  for (const problem of problems) {
    const k = key(problem)
    if (! (k in work)) work[k] = { score: 0, state: null }  
  } 

  
  const returnToWorkURLSpan =  document.getElementById('returnToWorkURL')
  returnToWorkURLSpan.textContent = assignmentData.returnToWorkURL
  
  document.getElementById('returnToWork').appendChild(createButton('hc-command', 'Copy', () => { 
    window.getSelection().selectAllChildren(returnToWorkURLSpan); 
    document.execCommand('copy');
    window.getSelection().removeAllRanges(); }))

  
  function sendingIframe(event) {
    for (const f of document.getElementsByClassName('exercise-iframe'))
      if (f.contentWindow === event.source) return f
    return undefined
  }

  function updateScoreDisplay(workKey) {
    if (!assignmentData.editable) return
    const score = work[workKey].score
    const button = document.getElementById('button-' + workKey)
    const text = button.textContent
    let index = text.indexOf(' (')
    if (index < 0) index = text.length
    button.textContent = text.substring(0, index) + ' (' + percent(score) + ')'
  }

  function adjustDocHeight(iframe, request) {
    const newHeight = request.param.docHeight;
    iframe.style.height = newHeight + 'px'
  }

  function restoreStateOfProblem(iframe, request) {
    let workKey = iframe.id.replace('problem-', '')   
    iframe.contentWindow.postMessage({ request, param: work[workKey].state }, '*');
    updateScoreDisplay(workKey);
  }

  async function sendScoreAndState(iframe, request) {    
    if (studentID === null || !assignmentData.editable) return // Viewing as instructor
    let workKey = iframe.id.replace('problem-', '')   
    work[workKey] = request.param
    updateScoreDisplay(workKey);     
    try {
      responseDiv.style.display = 'none'
      responseDiv.title = ''
      response = await postData(assignmentData.workUpdateURL, work) 
      responseDiv.textContent = `Score: ${percent(score(problems, work))}`
      responseDiv.title = `Submitted at ${response.submittedAt}`
      responseDiv.style.display = 'block'
    } catch (e) {
      responseDiv.textContent = `Error: ${e.message}` 
      responseDiv.style.display = 'block'
    }  
  }
    
  function activateButtons() {
    for (const btn of buttonDiv.children) {
      btn.classList.remove('hc-disabled')
    }
    buttonDiv.children[0].classList.add('active')
    const iframeID = buttonDiv.children[0].id.replace('button', 'problem')       
    document.getElementById(iframeID).style.display = 'block'                   
  }
  
  window.addEventListener("message", event => {
    let iframe = sendingIframe(event)
    if (event.data.query === 'docHeight') 
      adjustDocHeight(iframe, event.data)
    else if (event.data.query === 'retrieve') 
      restoreStateOfProblem(iframe, event.data)      
    else if (event.data.query === 'send') 
      sendScoreAndState(iframe, event.data)
  }, false);
  
  document.getElementById('switchID').appendChild(createButton('hc-command', 'Switch to this ID', () => { 
      const newccid =  document.getElementById('newccid').value
      if (/^(([aeiouy][bcdfghjklmnpqrstvwxz]){2}|([bcdfghjklmnpqrstvwxz][aeiouy]){2})(-(([aeiouy][bcdfghjklmnpqrstvwxz]){2}|([bcdfghjklmnpqrstvwxz][aeiouy]){2})){3}$/.test(newccid)) {
        location = `${location.href.split(/[?#]/)[0]}?newid=${newccid}`                
      } else {
        document.getElementById('response').textContent = 'Not a valid CodeCheck ID'
        document.getElementById('response').style.display = 'block'
      }        
    }))
  document.getElementById('clearID').appendChild(createButton('hc-command', 'Clear ID now', () => {
      location = `${location.href.split(/[?#]/)[0]}?newid=`    
    }))
      
  buttonDiv.id = 'buttons'
  document.body.appendChild(buttonDiv)
  
  let allWeightsSame = true
  for (let i = 1; allWeightsSame && i < problems.length; i++) 
    allWeightsSame = Math.abs(problems[0].weight - problems[i].weight) < 0.001   

  for (let i = 0; i < problems.length; i++) {
    const workKey = key(problems[i])
    const iframe = document.createElement('iframe')    
    iframe.id = 'problem-' + workKey
    iframe.className = 'exercise-iframe'
    iframe.src = problems[i].URL 
    document.body.appendChild(iframe)
    iframe.style.display = 'none'
    const action = () => {
      if (!savedCopyCheckbox.checked) return
      for (const f of document.getElementsByClassName('exercise-iframe'))
        if (f !== iframe) f.style.display = 'none'
      iframe.style.display = 'block'
      for (const btn of buttonDiv.children)
        if (btn !== button)
          btn.classList.remove('active')
      button.classList.add('active')  
    }    

    const button = createButton('hc-step', '' + (i + 1), action)
    button.classList.add('hc-disabled')
    button.id = 'button-' + workKey
    buttonDiv.appendChild(button)
    button.textContent = "" + (i + 1)
    if (!allWeightsSame) button.title = `Weight: ${percent(problems[i].weight)}` 
  }
  
  if (assignmentData.editable) { 
    if (assignmentData.editKeySaved) {
      activateButtons()
      document.getElementById('savedcopy').style.display = 'none'
    } else {  
      savedCopyCheckbox.checked = false
    }
  } else { // Instructor view
    for (const e of document.querySelectorAll('.studentInstructions')) e.style.display = 'none'
    if ('cloneURL' in assignmentData) 
      document.getElementById('abovebuttons').appendChild(createButton('hc-command', 'Clone', () => {
        window.open(assignmentData.cloneURL, '_blank')        
      }))
    savedCopyCheckbox.checked = true
    activateButtons()
  }
  
  
  
  savedCopyCheckbox.addEventListener('change', () => {    
    if (savedCopyCheckbox.checked) {
      activateButtons()
    } else {
      for (const btn of buttonDiv.children) {
        btn.classList.add('hc-disabled')
        btn.classList.remove('active')
      }
      for (const f of document.getElementsByClassName('exercise-iframe'))
        f.style.display = 'none'
    }
  })  
})