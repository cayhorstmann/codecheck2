document.addEventListener('DOMContentLoaded', function () {
	
  // For embedding CodeCheck in an iframe

  function getScore() {
    var repo = document.querySelector('input[name=repo]').getAttribute('value');
    var problem = document.querySelector('input[name=problem]').getAttribute('value');
    var scoreText = document.getElementById('codecheck-submit-response').score;
    var correct = 0;
    var maxscore = 1; // default maxscore. not 0 to avoid divide by zero
    if (scoreText !== undefined && scoreText !== '0' && scoreText.length > 0) {
      correct = scoreText.split('/')[0];
      maxscore = scoreText.split('/')[1];
    }
    return {correct: correct, errors: 0, maxscore: maxscore, repo: repo, problem: problem};
  }

  function receiveMessage(event) {
    const origin = event.origin || event.originalEvent.origin;
    // For Chrome, the origin property is in the event.originalEvent object.
    // TODO: Filter origin?
    if (event.data.response) return; // It's a response
    else if (event.data.query === 'docHeight') {
      const body = document.body
      const html = document.documentElement;
      const fudge = 50;
      const height = Math.max( body.scrollHeight, body.offsetHeight,
                               html.clientHeight, html.scrollHeight, html.offsetHeight ) + fudge;
      event.source.postMessage({docHeight: height, request: event.data}, '*' );
    } else if (event.data.query === 'restoreState') {
      const studentWork = event.data.state;

      for (let i = 0; i < studentWork.length; i++) {
        const problemName = studentWork[i].problemName;
        const studentCode = studentWork[i].code;

        // Need to get the textarea with the given id, then find the ace editor from there
        const editorDiv = document.getElementById(problemName);
        const editor = ace.edit(editorDiv);
        editor.setValue(studentCode);
      }
    } else if (event.data.query === 'getContent') {
      const problems = document.querySelectorAll('body > form');

      let studentWork = [];
      var editorDivs = document.getElementsByClassName('editor');
      for (var i = 0; i < editorDivs.length; i++) {
        let editor = ace.edit(editorDivs[i]);
        if (!editorDivs[i].classList.contains('readonly'))
          studentWork.push({problemName: editorDivs[i].getAttribute('id'), code: editor.getValue()});
      }
      

      const response = { request: event.data, score: getScore(), state: studentWork };
      event.source.postMessage(response, event.origin);        
    }
    else if (event.data.query === undefined) { // Engage
      var response = getScore();
      response.request = event.data;
      event.source.postMessage(response, event.origin);        
    }
  }

  window.addEventListener('message', receiveMessage, false);

  // Set up Ace editors
	
  var files = document.getElementsByClassName('file');
  for (var i = 0; i < files.length; i++) {
    let editorDivs = files[i].getElementsByClassName('editor');
    let editors = [];
    for (var k = 0; k < editorDivs.length; k++)
      editors.push(ace.edit(editorDivs[k]));
    for (var k = 0; k < editors.length; k++) {
      if (editorDivs[k].getAttribute('id').indexOf('.java-')!=-1) {
	editors[k].getSession().setMode('ace/mode/java');
      } else if(editorDivs[k].getAttribute('id').indexOf('.cpp-')!=-1) {
	editors[k].getSession().setMode('ace/mode/c_cpp');
      } else if(editorDivs[k].getAttribute('id').indexOf('.py-')!=-1) {
	editors[k].getSession().setMode('ace/mode/python');
      } else {
	editors[k].getSession().setMode('ace/mode/text');
      }
      editors[k].setOption('autoScrollEditorIntoView', true);
      editors[k].setOption('displayIndentGuides', false);
      // TODO: These give error "misspelled option"
      // editors[k].setOption('enableBasicAutocompletion', true);
      // editors[k].setOption('enableLiveAutocompletion', false);
      // editors[k].setOption('enableSnippets', true);
      editors[k].setOption('tabSize', 3);
      editors[k].setOption('useWorker', true);
      editors[k].setOption('highlightActiveLine', false);
      editors[k].setOption('highlightGutterLine', false);
      editors[k].setOption('showFoldWidgets', false);
      editors[k].setOption('newLineMode', 'unix');
      editors[k].setOption('fontSize', '1em');
      
      
      if(editorDivs[k].classList.contains('readonly')){
	editors[k].setReadOnly(true);
	// https://stackoverflow.com/questions/32806060/is-there-a-programmatic-way-to-hide-the-cursor-in-ace-editor
	editors[k].renderer.$cursorLayer.element.style.display = 'none'
	editors[k].setTheme('ace/theme/kuroir');
      } else {
	editors[k].setTheme('ace/theme/chrome');
      }
    }
    
    var update = function() {
      var totalLines = 0;
      for (var k = 0; k < editors.length; k++) {
        var editorSession = editors[k].getSession()
        editorSession.clearAnnotations()
	editorSession.setOption('firstLineNumber', totalLines + 1);
	var lines = editors[k].getSession().getDocument().getLength();
	editorDivs[k].style.height = editors[k].renderer.lineHeight * lines + "px";
	editors[k].resize();
	totalLines += lines;
      }
    };
    for (var k = 0; k < editors.length; k++) {
      editors[k].on('change', update);
    }
    update();
  }

  // Form submission

  function inIframe() {
    try {
      return window.self !== window.top;
    } catch (e) {
      return true;
    }	
  }

  function highlightLine(file, line, message) {
    var totalLines = 0;
    let fileDiv = document.getElementById(file);
    if (fileDiv == null) return // This happens if there is an error in a tester
    let editorDivs = fileDiv.getElementsByClassName('editor');
    let editors = [];
    for (var k = 0; k < editorDivs.length; k++)
      editors.push(ace.edit(editorDivs[k]));
    for (var k = 0; k < editors.length; k++) {
      let editorSession = editors[k].getSession();
      var length = editorSession.getDocument().getLength() 
      totalLines += length;
      if (totalLines >= line) {
        var annotations = editorSession.getAnnotations()
        annotations.push({
          row: line - (totalLines - length) - 1, // ace editor lines are 0-indexed
          text: message,
          type: "error"
        })
        editorSession.setAnnotations(annotations);
        return;
      }
    }    
  };

  function clearErrorAnnotations () {
    let editorDivs = document.getElementsByClassName('editor');
    for (var k = 0; k < editorDivs.length; k++)
      ace.edit(editorDivs[k]).getSession().clearAnnotations()
  };

  function successfulSubmission(data) {
    var submitButton = document.getElementById('submit');        
    var codecheckSubmitResponse = document.getElementById('codecheck-submit-response')
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
      for (var i = 0; i < data['errors'].length; i++) {
        var error = data['errors'][i]; 
        highlightLine(error['file'], error['line'], error['message']); }
    }
  }

  var form = document.getElementsByTagName('form')[0]
  form.addEventListener('submit', function(e) {
    e.preventDefault();
    var submitButton = document.getElementById('submit');        
    submitButton.setAttribute('disabled', 'disabled');
    clearErrorAnnotations();
    var codecheckSubmitResponse = document.getElementById('codecheck-submit-response')
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
    
    var xhr = new XMLHttpRequest()
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
})	


