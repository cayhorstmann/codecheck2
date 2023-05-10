document.addEventListener('DOMContentLoaded', function () {
  // TODO: Legacy 
  // For embedding CodeCheck in an iframe
  
  function getScore() { // TODO: Remove when decommissioning v1
    let repo = document.querySelector('input[name=repo]').getAttribute('value');
    let problem = document.querySelector('input[name=problem]').getAttribute('value');
    let scoreText = document.getElementById('codecheck-submit-response').score;
    let correct = 0;
    let maxscore = 1; // default maxscore. not 0 to avoid divide by zero
    if (scoreText !== undefined && scoreText !== '0' && scoreText.length > 0) {
      correct = scoreText.split('/')[0];
      maxscore = scoreText.split('/')[1];
    }
    return {correct: correct, errors: 0, maxscore: maxscore, repo: repo, problem: problem};
  }
  
  function getNumericScore() {
    let scoreText = document.getElementById('codecheck-submit-response').score;
    let correct = 0;
    let maxscore = 1; // default maxscore. not 0 to avoid divide by zero
    if (scoreText !== undefined && scoreText !== '0' && scoreText.length > 0) {
      correct = scoreText.split('/')[0];
      maxscore = scoreText.split('/')[1];
    }
    return correct / maxscore   
  }

  function restoreStudentWork(studentWork) {
    if (studentWork === null) return 
    for (let i = 0; i < studentWork.length; i++) {
      const problemName = studentWork[i].problemName;
      const studentCode = studentWork[i].code;

      // Need to get the textarea with the given id, then find the ace editor from there
      const editorDiv = document.getElementById(problemName);
      const editor = ace.edit(editorDiv);
      editor.setValue(studentCode);
    }
  }
  
  function getStudentWork() {
    let studentWork = [];
    let editorDivs = document.getElementsByClassName('editor');
    for (let i = 0; i < editorDivs.length; i++) {
      let editor = ace.edit(editorDivs[i]);
      if (!editorDivs[i].classList.contains('readonly'))
        studentWork.push({problemName: editorDivs[i].getAttribute('id'), code: editor.getValue()});
    }
    return studentWork      
  }

  function receiveMessage(event) {
    if (event.data.request) { // It's a response
      const request = event.data.request    
      if (request.query === 'retrieve') { // LTIHub v2
        restoreStudentWork(event.data.param)                
      }
    }
    else if (event.data.query === 'docHeight') { // All of these are LTIHub v1
      const body = document.body
      const html = document.documentElement;
      const fudge = 50;
      const height = Math.max( body.scrollHeight, body.offsetHeight,
                               html.clientHeight, html.scrollHeight, html.offsetHeight ) + fudge;
      document.body.style.overflow = 'auto'; 
      event.source.postMessage({docHeight: height, request: event.data}, '*' );
    } else if (event.data.query === 'restoreState') {
      document.body.style.overflow = 'auto'; 
      restoreStudentWork(event.data.state)      
    } else if (event.data.query === 'getContent') {
      const studentWork = getStudentWork()
      const response = { request: event.data, score: getScore(), state: studentWork };
      event.source.postMessage(response, event.origin);        
    }
  }

  window.addEventListener('message', receiveMessage, false);

  // Set up Ace editors
  
  let files = document.getElementsByClassName('file');
  for (let i = 0; i < files.length; i++) {
    let editorDivs = files[i].getElementsByClassName('editor');
    let editors = [];
    for (let k = 0; k < editorDivs.length; k++)
      editors.push(ace.edit(editorDivs[k]));
    for (let k = 0; k < editors.length; k++) {
      let divId = editorDivs[k].getAttribute('id')
      let session = editors[k].getSession()  
      if (divId.indexOf('.java-')!=-1) {
        session.setMode('ace/mode/java');
      } else if(divId.indexOf('.cpp-')!=-1 || divId.indexOf('.c-')!=-1 || divId.indexOf('.h-')!=-1) {
        session.setMode('ace/mode/c_cpp');
      } else if(divId.indexOf('.py-')!=-1) {
        session.setMode('ace/mode/python');
      } else {
        session.setMode('ace/mode/text');
      }
      editors[k].setOption('autoScrollEditorIntoView', true);
      editors[k].setOption('displayIndentGuides', false);
      // TODO: These give error "misspelled option"
      // editors[k].setOption('enableBasicAutocompletion', true);
      // editors[k].setOption('enableLiveAutocompletion', false);
      // editors[k].setOption('enableSnippets', true);
      editors[k].setOption('tabSize', 3);
      // TODO: Compute tab size from source
      // editors[k].setUseSoftTabs(false); // TODO
      editors[k].setOption('useWorker', true);
      editors[k].setOption('highlightActiveLine', false);
      editors[k].setOption('highlightGutterLine', false);
      editors[k].setOption('showFoldWidgets', false);
      editors[k].setOption('newLineMode', 'unix');
      editors[k].setOption('fontSize', '1em');
      editors[k].setOption('scrollSpeed', 0);      
      
      if(editorDivs[k].classList.contains('readonly')){
  editors[k].setReadOnly(true);
  // https://stackoverflow.com/questions/32806060/is-there-a-programmatic-way-to-hide-the-cursor-in-ace-editor
  editors[k].renderer.$cursorLayer.element.style.display = 'none'
  editors[k].setTheme('ace/theme/kuroir');
      } else {
  editors[k].setTheme('ace/theme/chrome');
      }
    }
    
    let update = function() {
      let totalLines = 0;
      for (let k = 0; k < editors.length; k++) {
        let editorSession = editors[k].getSession()
        editorSession.clearAnnotations()
  editorSession.setOption('firstLineNumber', totalLines + 1);
  let lines = editors[k].getSession().getDocument().getLength();
        editorDivs[k].style.height = (editors[k].renderer.lineHeight * lines) + "px";
  editors[k].resize();
        let aceScroller = editorDivs[k].getElementsByClassName('ace_scroller')[0]
        aceScroller.style.bottom = '0px'
        // this is the scrolled area, not the scroll bar
        // we are hiding the scroll bars in css, but ace doesn't seem to take that into account
  totalLines += lines;
      }
    }
    for (let k = 0; k < editors.length; k++) {
      editors[k].on('change', update);
    }
    window.addEventListener('resize', function() { setTimeout(update, 1000)})
    update();
  }

  // Form submission

  function inIframe() {
    try {
      return window.self !== window.top
    } catch (e) {
      return true
    } 
  }

  function highlightLine(file, line, message) {
    let totalLines = 0
    let fileDiv = document.getElementById(file);
    if (fileDiv == null) return // This happens if there is an error in a tester
    let editorDivs = fileDiv.getElementsByClassName('editor')
    let editors = []
    for (let k = 0; k < editorDivs.length; k++)
      editors.push(ace.edit(editorDivs[k]))
    for (let k = 0; k < editors.length; k++) {
      let editorSession = editors[k].getSession();
      let length = editorSession.getDocument().getLength() 
      totalLines += length
      if (totalLines >= line) {
        let annotations = editorSession.getAnnotations()
        annotations.push({
          row: line - (totalLines - length) - 1, // ace editor lines are 0-indexed
          text: message,
          type: "error"
        })
        editorSession.setAnnotations(annotations);
        return
      }
    }    
  }

  function clearErrorAnnotations () {
    let editorDivs = document.getElementsByClassName('editor');
    for (let k = 0; k < editorDivs.length; k++)
      ace.edit(editorDivs[k]).getSession().clearAnnotations()
  }
  
  function questionID(form) {
    let inputs = form.getElementsByTagName('input');
    let problem = ''
    let repo = ''
    for (const input of inputs) {
      let name = input.getAttribute('name')
      if (name === 'problem')  
        problem = input.getAttribute('value')
      else if (name === 'repo')
        repo = input.getAttribute('value')
    }
    if (repo === 'wiley') return problem  
    else return window.location.href
  }
  
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

  let form = document.getElementsByTagName('form')[0]
  let qid = questionID(form)

  function successfulSubmission(data) {
    let submitButton = document.getElementById('submit');        
    let codecheckSubmitResponse = document.getElementById('codecheck-submit-response')
    submitButton.removeAttribute('disabled');
    codecheckSubmitResponse.innerHTML = data['report']
    if (!inIframe()) { // No download button in iframe (Engage)
      if(/(Version)\/(\d+)\.(\d+)(?:\.(\d+))?.*Safari\//.test(navigator.userAgent)) {
  codecheckSubmitResponse.innerHTML += '<div>Download not supported on Safari. Use Firefox or Chrome!</div>'      
      }
      else {
  codecheckSubmitResponse.innerHTML += 
    '<div class="download">'
    + '<button onclick="download(\'data:application/octet-stream;base64,' + data.zip + '\', \'' + data.metadata.ID + '.signed.zip\', \'application/octet-stream\')">Download Report</button></div>'
      }
    }
    codecheckSubmitResponse.score = data['score']

    clearErrorAnnotations();                
    if ('errors' in data) {
      for (let i = 0; i < data['errors'].length; i++) {
        let error = data['errors'][i]; 
        highlightLine(error['file'], error['line'], error['message']); }
    }
    
    if (inIframe()) {      
      const param = { qid, state: getStudentWork(), score: getNumericScore() }
      const data = { query: 'send', param }
      console.log('Posting to parent', data)
      window.parent.postMessage(data, '*' )
    }
  }


  form.addEventListener('submit', function(e) {
    e.preventDefault();
    let submitButton = document.getElementById('submit');        
    submitButton.setAttribute('disabled', 'disabled');
    clearErrorAnnotations();
    let codecheckSubmitResponse = document.getElementById('codecheck-submit-response')
    codecheckSubmitResponse.innerHTML = '<p>Submitting...</p>'
    let params = {}
    let inputs = form.getElementsByTagName('input');
    for (let i = 0; i < inputs.length; i++) {
      let name = inputs[i].getAttribute('name')
      if (name !== null) 
        params[name] = inputs[i].getAttribute('value')
    }
    
    let files = document.getElementsByClassName('file');
    for (let i = 0; i < files.length; i++) {
      let allContent = "";
      let editorDivs = files[i].getElementsByClassName('editor');
      for (let k = 0; k < editorDivs.length; k++) {
        if (k > 0) allContent += "\n"
        allContent += ace.edit(editorDivs[k]).getValue();
      }
      let filename = files[i].getAttribute('id');
      params[filename] = allContent
    }
    
    let xhr = new XMLHttpRequest()
    xhr.timeout = 300000 // 5 minutes
    xhr.open('POST', '/checkNJS');    
    xhr.setRequestHeader('Content-Type', 'application/json');
    xhr.onload = function() {
      if (xhr.status === 200) successfulSubmission(JSON.parse(xhr.responseText))
      else {
        submitButton.removeAttribute('disabled');
        codecheckSubmitResponse.innerHTML = 
          '<div>Error Status: ' + xhr.status + ' ' + xhr.statusText + '</div>\n' +
          '<div>Error Response: ' + xhr.responseText + '</div>\n';
      }
    }
    xhr.send(JSON.stringify(params))
  })
  
  if (inIframe()) {
    // document.body.style.height = '100%'
    // document.body.style.overflow = 'hidden'
    // ResizeObserver did not work          
    const mutationObserver = new MutationObserver(sendDocHeight);
    mutationObserver.observe(document.documentElement, { childList: true, subtree: true })    
    
    const data = { query: 'retrieve', param: { qid } }  
    console.log('Posting to parent', data)
    window.parent.postMessage(data, '*' )
  }  
})  


