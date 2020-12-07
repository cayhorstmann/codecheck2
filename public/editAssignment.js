window.addEventListener('DOMContentLoaded', () => {
  function format(problems) {
    let result = ''
    for (let i = 0; i < problems.length; i++) {
      if (i > 0) result += '---\n'
      const group = problems[i]
      for (const problem of group) {
        result += 'qid' in problem ? problem.qid : problem.URL
        if (problem.weight != 1) result += ' ' + percent(problem.weight)
        if ('title' in problem) result += ' ' + problem.title 
        result += '\n'
      }      
    }
    return result
  }  
  
  const responseDiv = document.getElementById('response')
  const urlsDl = document.getElementById('urls')
  urlsDl.style.display = 'none'
  if ('problems' in assignment)  
    document.getElementById('problems').value = format(assignment.problems)
  if (askForDeadline) {
    if ('deadlineDate' in assignment)  
      document.getElementById('deadlineDate').value = assignment.deadlineDate
    if ('deadlineTime' in assignment)  
      document.getElementById('deadlineTime').value = assignment.deadlineTime
  } else {  
    const deadlineDiv = document.getElementById('deadlineDiv')
    deadlineDiv.style.display = 'none'
  }
  
  const privateURLSpan = document.getElementById('privateURL')
  privateURLSpan.parentNode.appendChild(createButton('hc-command', 'Copy', () => { 
    window.getSelection().selectAllChildren(privateURLSpan); 
    document.execCommand('copy');
    window.getSelection().removeAllRanges(); }))

  const viewSubmissionsDiv = document.getElementById('viewSubmissionsDiv')
  if ('viewSubmissionsURL' in assignment) {
    viewSubmissionsDiv.appendChild(createButton('hc-command', 'View submissions', () => {
      window.location.href = assignment.viewSubmissionsURL
    }))  
  } else {
    viewSubmissionsDiv.style.display = 'none'  
  }
    
  const submitButton = createButton('hc-command', 'Save', async () => {
    let request = {
        assignmentID: assignment.assignmentID,
        editKey: assignment.editKey, // undefined when cloned
        problems: document.getElementById('problems').value,
      }
    if (askForDeadline) {
      request.deadlineDate = document.getElementById('deadlineDate').value
      request.deadlineTime = document.getElementById('deadlineTime').value        
    }
    urlsDl.style.display = 'none'
    submitButton.disabled = true
    responseDiv.textContent = ''
    try {
      let response = await postData(assignment.saveURL, request)
      assignment.assignmentID = response.assignmentID
      if ('publicURL' in response) {
        document.getElementById('publicURL').textContent = response.publicURL
        document.getElementById('privateURL').textContent = response.privateURL
        urlsDl.style.display = 'block'
      }
      if ('assignmentURL' in response) {
        if ('launchPresentationReturnURL' in assignment) {
          const params = new URLSearchParams()
          params.append('return_type', 'lti_launch_url')
          params.append('url', response.assignmentURL)
          const url =  assignment.launchPresentationReturnURL
            + (assignment.launchPresentationReturnURL.includes("?") ? "&" : "?")
            + params.toString()
          window.location.href = url
        } else {
          responseDiv.textContent = `Assignment URL: ${response.assignmentURL}`
        }
      }    
    } catch (e) {
      responseDiv.textContent = e.message            
    }
    submitButton.disabled = false    
  })
  document.getElementById('saveButtonDiv').appendChild(submitButton) 
})
