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

  let field = document.querySelector('#deadlineDate')
  deadlineDate.addEventListener('input', function() {
    let date = field.value
    document.getElementById('deadlineDate').value = date
    const local = document.getElementById('deadlineLocal')
    const utc = document.getElementById('deadlineUTC')
    if (date === '') {
      local.style.display = "none"
      utc.style.display = "none"
    } else {
      let date = document.getElementById('deadlineDate').value
      let deadlineLocal = new Date(date)
      let deadlineUTC = new Date(deadlineLocal).toUTCString() 
      local.textContent = deadlineLocal
      utc.textContent = deadlineUTC + " (UTC)"
      local.style.display = "block"
      utc.style.display = "block"
    }
  })

  const responseDiv = document.getElementById('response')
  if ('problems' in assignment)  
    document.getElementById('problems').value = format(assignment.problems)
  if (askForDeadline) {
    if ('deadline' in assignment) { // an ISO 8601 string like "2020-12-01T23:59:59Z"
      document.getElementById('deadlineDate').value = assignment.deadline
    } 
  } else {  
    const deadlineDiv = document.getElementById('deadlineDiv')
    deadlineDiv.style.display = 'none'
  }
  
  const submitButton = createButton('hc-command', 'Save', async () => {
    let request = {
        assignmentID: assignment.assignmentID,
        editKey: assignment.editKey, // undefined when cloned
        problems: document.getElementById('problems').value,
      }
      
    if (askForDeadline) {
      if(document.getElementById('deadlineDate').value !== ''){
        request.deadline = new Date(document.getElementById('deadlineDate').value).toISOString()
      }
    }
    submitButton.disabled = true
    responseDiv.style.display = 'none'
    try {
      let response = await postData(assignment.saveURL, request)
      if ('launchPresentationReturnURL' in assignment) {
        const params = new URLSearchParams()
        params.append('return_type', 'lti_launch_url')
        params.append('url', response.launchURL)
        const url =  assignment.launchPresentationReturnURL
          + (assignment.launchPresentationReturnURL.includes("?") ? "&" : "?")
          + params.toString()
        window.location.href = url
      } else {
        window.location.href = response.viewAssignmentURL
      }
    } catch (e) {
      responseDiv.textContent = e.message           
      responseDiv.style.display = 'block'
    }
    submitButton.disabled = false    
  })
  document.getElementById('saveButtonDiv').appendChild(submitButton) 
})
