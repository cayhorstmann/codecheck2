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
  if (assignmentData !== undefined) { 
    document.getElementById('problems').value = format(assignmentData.problems)
    document.getElementById('deadlineDate').value = assignmentData.deadlineDate
    document.getElementById('deadlineTime').value = assignmentData.deadlineTime
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
      request = {
        problems: document.getElementById('problems').value,
        deadlineDate: document.getElementById('deadlineDate').value,
        deadlineTime: document.getElementById('deadlineTime').value,
      }
      if (assignmentID !== '') request.assignmentID = assignmentID       
      response = await postData('/saveAssignment', request)
      if (response.error !== undefined) {
        responseDiv.textContent = `Error: ${response.error}`
        responseDiv.style.display = 'block'
      } else {
        document.getElementById('publicURL').textContent = response.publicURL
        document.getElementById('privateURL').textContent = response.privateURL
        urlsDl.style.display = 'block'
        assignmentID = response.assignmentID
      }
    } catch (e) {
      responseDiv.textContent = `Error: ${e.message}`            
      responseDiv.style.display = 'block'
    }
    submitButton.disabled = false    
  })
  document.getElementById('saveButtonDiv').appendChild(submitButton) 
})