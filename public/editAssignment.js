window.addEventListener('DOMContentLoaded', () => {
  function format(problems) {
    let result = ''
    for (let i = 0; i < problems.length; i++) {
      if (i > 0) result += '---\n'
      const group = problems[i]
      let equalWeights = true
      for (let j = 1; equalWeights && j < group.length; j++) 
        equalWeights = group[0].weight === group[j].weight 
      for (const problem of group) {
        result += 'qid' in problem ? problem.qid : problem.URL
        if (!equalWeights) result += ' ' + percent(problem.weight)
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
    
  const submitButton = createButton('hc-command', 'Save', async () => {
    submitButton.disabled = true
    responseDiv.style.display = 'none'
    urlsDl.style.display = 'none'
    try {
      let request = {
          assignmentID: assignment.assignmentID,
          editKey: assignment.editKey, // undefined when cloned
          problems: document.getElementById('problems').value,
        }
      if (askForDeadline) {
        request.deadlineDate = document.getElementById('deadlineDate').value
        request.deadlineTime = document.getElementById('deadlineTime').value        
      }
      /*
      if ('launchPresentationReturnURL' in assignment)
        request.launchPresentationReturnURL = assignment.launchPresentationReturnURL
      */
      let response = await postData(assignment.saveURL, request)
      if (response.error !== undefined) {
        responseDiv.textContent = `Error: ${response.error}`
        responseDiv.style.display = 'block'
      } else {
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
              + (assignment.launchPresentationReturnURL.contains("?") ? "&" : "?")
              + params.toString()
            window.location.href = url
          } else {
            responseDiv.textContent = `Assignment URL: ${response.assignmentURL}`
            responseDiv.style.display = 'block'          
          }
        }
        /*
      if (launchPresentationReturnURL != null) {
      launchPresentationReturnURL =                
          + "return_type=lti_launch_url"
          + "&url=" + URLEncoder.encode(assignmentURL, "UTF-8"); // TODO StandardCharsets.UTF_8 
      new URL(launchPresentationReturnURL).openStream().close();
      // TODO: Capture output from above and return?
      }
  */
      }
    } catch (e) {
      responseDiv.textContent = `Error: ${e.message}`            
      responseDiv.style.display = 'block'
    }
    submitButton.disabled = false    
  })
  document.getElementById('saveButtonDiv').appendChild(submitButton) 
})
