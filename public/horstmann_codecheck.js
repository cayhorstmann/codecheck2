window.horstmann_codecheck = {
  setup: [],
};

if (typeof ace !== 'undefined') { ace.config.set('themePath', 'script'); }

window.addEventListener('load', function () {
  'use strict';

  function initElement(element, setup, prefix) {
    let form = undefined
    let response = undefined
    let submitButton = undefined
    let downloadButton = undefined

    function restoreState(element, state) { 
      if (state === null) return;
      let studentWork = Array.isArray(state) ? state : state.studentWork
      for (let i = 0; i < studentWork.length; i++) {
        let editorDiv = document.getElementById(
          prefix + studentWork[i].problemName)
        let editor = ace.edit(editorDiv)
        editor.setValue(studentWork[i].code)
        editor.clearSelection()
      }
      if (state.hasOwnProperty('scoreText')) {
        response.textContent = 'Score: ' + state.scoreText
      }
    }
  
    function appendRequiredFile(fileName, directoryPrefix) {
      let fileDiv = document.createElement('div')
      fileDiv.setAttribute('id', prefix + fileName)
      fileDiv.setAttribute('name', fileName)
      fileDiv.classList.add('file')
      let filenameDiv = document.createElement('div')
      filenameDiv.textContent = directoryPrefix + fileName
      filenameDiv.classList.add('codecheckFilename')
      fileDiv.appendChild(filenameDiv)
      let numArr = setup.requiredFiles[fileName];
      let editorCount = 0;
      for (let i = 0; i < numArr.length; i++) {
        let codeString = numArr[i];
        if (codeString === null)
          continue;
      
        let readonly = i % 2 !== 0; 
        let elName = fileName + '-' + (++editorCount);
        let editorDiv = document.createElement('div')
        editorDiv.setAttribute('name', elName)
        editorDiv.setAttribute('id', prefix + elName)
        editorDiv.classList.add('editor')
        editorDiv.textContent = codeString.replace(/\r?\n$/, '');
        if (readonly)
          editorDiv.setAttribute('readonly', 'readonly');
        if (fileName === 'Input')
        editorDiv.classList.add('input')
      
        fileDiv.appendChild(editorDiv);
      }
      form.appendChild(fileDiv);
    }
  
    function initUI() { 
      form = document.createElement('form')
      let submitDiv = document.createElement('div')
      submitDiv.classList.add('codecheckSubmit')
      let submitButtonLabel = _('CodeCheck')
      
      let directoryPrefix = setup.prefix ? setup.prefix + '/' : '';
      let inputPresent = false;
      let orderedFileNames = setup.order ? setup.order.split(/\s*,\s*/) : []
      let requiredFileNames = []
      let useFileNames = []
      for (let i = 0; i < orderedFileNames.length; i++) {
        let fileName = orderedFileNames[i];
        if (setup.requiredFiles[fileName])
          requiredFileNames.push(fileName);
        else if (setup.useFiles[fileName])
          useFileNames.push(fileName)
      }
      
      // TODO: Iterate by sort order?
      for (let fileName in setup.requiredFiles) { 
        if (fileName === 'Input') {
          submitButtonLabel = _('Run')
          // Don't provide an edit box if no input required
          if (setup.requiredFiles['Input'][0].trim().length > 0)
            inputPresent = true;
          else {
            let hiddenInput = document.createElement('input')
            hiddenInput.setAttribute('type', 'hidden')
            hiddenInput.setAttribute('name', 'Input')
            hiddenInput.setAttribute('value', '')
            submitDiv.appendChild(hiddenInput)
          }
        }
        else 
          if (requiredFileNames.indexOf(fileName) < 0) requiredFileNames.push(fileName)
      }
      
      // TODO: Iterate by sort order?
      for (let fileName in setup.useFiles)
        if (useFileNames.indexOf(fileName) < 0) useFileNames.push(fileName)
      
      for (let i = 0; i < requiredFileNames.length; i++) 
        appendRequiredFile(requiredFileNames[i], directoryPrefix);      
      if (inputPresent) appendRequiredFile('Input', '')
    
      for (let i = 0; i < useFileNames.length; i++) {
        let fileName = useFileNames[i]
        
        let editorDiv = document.createElement('div')
        editorDiv.classList.add('editor')
        editorDiv.textContent = setup.useFiles[fileName].replace(/\r?\n$/, '');
        let editor = ace.edit(editorDiv)
        
        let fileObj = document.createElement('div')
        fileObj.classList.add('codecheckUseFile')
        let filenameDiv = document.createElement('div')
        filenameDiv.classList.add('codecheckFilename')
        filenameDiv.textContent = directoryPrefix + fileName
        fileObj.appendChild(filenameDiv)
        fileObj.appendChild(editorDiv)
        setupAceEditor(editorDiv, editor, fileName, /*readonly*/ true)        
        form.appendChild(fileObj)
      }  
      
      submitButton = document.createElement('span')
      submitButton.textContent = submitButtonLabel
      submitButton.classList.add('hc-button')
      submitButton.classList.add('hc-start')
      submitButton.tabIndex = 0 
      submitDiv.appendChild(submitButton);

      
      let resetButton = document.createElement('span')
      resetButton.textContent = _('Reset')
      resetButton.classList.add('hc-button')
      resetButton.classList.add('hc-start')
      resetButton.tabIndex = 0
      submitDiv.appendChild(resetButton);

      if ('download' in horstmann_config) {
        downloadButton = document.createElement('span')
        downloadButton.textContent = _('Download')
        downloadButton.classList.add('hc-button')
        downloadButton.classList.add('hc-start')
        downloadButton.tabIndex = 0
        downloadButton.style.display = 'none'
        downloadButton.addEventListener('click', () => {
          horstmann_config.download('data:application/octet-stream;base64,' + downloadButton.data.zip, downloadButton.data.metadata.ID + '.signed.zip', 'application/octet-stream')
        })
        submitDiv.appendChild(downloadButton);
      }
      
      let input = document.createElement('input')
      input.setAttribute('type', 'hidden')
      input.setAttribute('name', 'repo')
      input.setAttribute('value', setup.repo)    
      submitDiv.appendChild(input)
      input = document.createElement('input')
      input.setAttribute('type', 'hidden')
      input.setAttribute('name', 'problem')
      input.setAttribute('value', setup.problem)
      submitDiv.appendChild(input)    
      form.appendChild(submitDiv);

      response = document.createElement('div')
      response.classList.add('codecheck-submit-response')
      form.appendChild(response)
      
      prepareSubmit(setup.url, prefix);

      element.appendChild(form)
      
      setupAceEditors(element);      
      
      let initialState = getState();
      resetButton.addEventListener('click', function() {
        restoreState(element, initialState)
        element.correct = 0;
        response.innerHTML = ''
        if (downloadButton !== undefined) downloadButton.style.display = 'none'
      })
    }

    // https://stackoverflow.com/questions/24963246/ace-editor-simply-re-enable-command-after-disabled-it
    function setCommandEnabled(editor, name, enabled) {
      var command = editor.commands.byName[name]
      if (!command.bindKeyOriginal) 
        command.bindKeyOriginal = command.bindKey
      command.bindKey = enabled ? command.bindKeyOriginal : null;
      editor.commands.addCommand(command);
      // special case for backspace and delete which will be called from
      // textarea if not handled by main commandb binding
      if (!enabled) {
        var key = command.bindKeyOriginal;
        if (key && typeof key == "object")
          key = key[editor.commands.platform];
        if (/backspace|delete/i.test(key))
          editor.commands.bindKey(key, "null")
      }
    }
    
    function setupAceEditor(editorDiv, editor, fileName, readonly) {
      let ext = fileName.match(/\.[^.]+$/)
      if (ext) ext = ext[0]
      if (ext === '.java') {
        editor.getSession().setMode('ace/mode/java');
      } else if (ext === '.cpp' || ext === '.h') {
        editor.getSession().setMode('ace/mode/c_cpp');
      } else if (ext === '.py') {
        editor.getSession().setMode('ace/mode/python');
      } else {
        editor.getSession().setMode('ace/mode/text');
      }
      editor.setOption('autoScrollEditorIntoView', true);
      editor.setOption('displayIndentGuides', false);
      editor.setOption('tabSize', 3);
      editor.setOption('useWorker', true);
      editor.setOption('highlightActiveLine', false);
      editor.setOption('highlightGutterLine', false);
      editor.setOption('showFoldWidgets', false);
      editor.setOption('newLineMode', 'unix');
      editor.setOption('showPrintMargin', false);
      editor.setFontSize(14);
      // https://stackoverflow.com/questions/28311086/modify-the-gutter-of-ajax-org-cloud9-editor-ace-editor
      editor.session.gutterRenderer =  {
        getWidth: function(session, lastLineNumber, config) {
          return 3 * config.characterWidth;
        },
        getText: function(session, row) {
          return session.getOption('firstLineNumber') + row;
        }
      };

      if (readonly) {
        editor.setReadOnly(true);
        // https://stackoverflow.com/questions/32806060/is-there-a-programmatic-way-to-hide-the-cursor-in-ace-editor
        editor.renderer.$cursorLayer.element.style.display = 'none'
        editor.setTheme('ace/theme/kuroir');
        let lines = editor.getSession().getDocument().getLength();
        editor.setOptions({
          minLines: lines,
          maxLines: lines
        });                
        // https://github.com/ajaxorg/ace/issues/266
        editor.textInput.getElement().tabIndex = -1
      } else {
        editor.setTheme('ace/theme/chrome');
        
        let editorHandleTextFieldEvents = function (e) {
          if (e.keyCode === 37 || e.keyCode === 39) // left or right
            e.stopPropagation();
        }

        editor.textInput.getElement().addEventListener('keyup', editorHandleTextFieldEvents)
        editor.textInput.getElement().addEventListener('keydown', editorHandleTextFieldEvents)

        editor.on('focus', function() {
          setCommandEnabled(editor, "indent", true)
          setCommandEnabled(editor, "outdent", true)
        })

        editor.commands.addCommand({
          name: "escape",
          bindKey: {win: "Esc", mac: "Esc"},
          exec: function() {
            setCommandEnabled(editor, "indent", false)
            setCommandEnabled(editor, "outdent", false)
          }
        });
      }
      let tas = editorDiv.getElementsByTagName('textarea')
      for (let i = 0; i < tas.length; i++) {
        tas[i].setAttribute('aria-label', 'Complete this code')
      }
    }
    
    function setupAceEditors() {
      let files = element.getElementsByClassName('file');
      for (let i = 0; i < files.length; i++) {
        let fileId = files[i].getAttribute('id')
        let editorDivs = files[i].getElementsByClassName('editor');
        let editors = [];
        for (let k = 0; k < editorDivs.length; k++)
          editors.push(ace.edit(editorDivs[k]));
        for (let k = 0; k < editors.length; k++) {
          let readonly = editorDivs[k].getAttribute('readonly')=='readonly'
          setupAceEditor(editorDivs[k], editors[k], fileId, readonly)
        }
        let update = function() {
          let totalLines = 0;
          for (let k = 0; k < editors.length; k++) {
            let editorSession = editors[k].getSession()
            editorSession.clearAnnotations()
            editorSession.setOption('firstLineNumber', totalLines + 1);
            let lines = editors[k].getSession().getDocument().getLength();
            editors[k].setOptions({
              minLines: lines,
              maxLines: lines
            });        
            editors[k].resize();
            totalLines += lines;
          }
        };
        for (let k = 0; k < editors.length; k++) {
          editors[k].on('change', update);
        }
        update();
      }
    }
    
    function highlightLine(file, line, message) {
      let totalLines = 0;
      let fileDiv = document.getElementById(prefix + file) 
      if (fileDiv === null) return // This happens if there is an error in a tester
      let editorDivs = fileDiv.getElementsByClassName('editor');
      let editors = [];
      for (let k = 0; k < editorDivs.length; k++)
        editors.push(ace.edit(editorDivs[k]));
      for (let k = 0; k < editors.length; k++) {
        let editorSession = editors[k].getSession();
        let length = editorSession.getDocument().getLength() 
        totalLines += length;
        if (totalLines >= line) {
          let annotations = editorSession.getAnnotations()
          annotations.push({
            row: line - (totalLines - length) - 1, // ace editor lines are 0-indexed
            text: message,
            type: 'error'
          })
          editorSession.setAnnotations(annotations);
          return;
        }
      }
    }
    
    function clearErrorAnnotations() { 
      let editorDivs = form.getElementsByClassName('editor');
      for (let k = 0; k < editorDivs.length; k++)
        ace.edit(editorDivs[k]).getSession().clearAnnotations()
    }
    
    function getState() {
      let studentWork = [];
      let editorDivs = element.getElementsByClassName('editor');
      for (let i = 0; i < editorDivs.length; i++) {
        let editor = ace.edit(editorDivs[i]);
        if (!editorDivs[i].classList.contains('readonly'))
          studentWork.push({problemName: editorDivs[i].getAttribute('name'), code: editor.getValue()});
      }
      return { studentWork: studentWork }
    }
  
    function setState(scoreText) {
      element.state = getState();
      element.state.scoreText = scoreText
      element.errors = 0;
      if (scoreText !== undefined  && scoreText !== '0' && scoreText.length > 0) {
        element.correct = scoreText.split('/')[0];
        element.maxscore = scoreText.split('/')[1];
      } else {
        element.correct = 0;
        element.maxscore = 1; // avoid divide by zero
      }
      let score = element.maxscore === 0 ? 0 : element.correct / element.maxscore    
      horstmann_config.score_change_listener && horstmann_config.score_change_listener(element, element.state, score)
    }

    function successfulSubmission(data) {
      let report = data['report']
      let start = report.indexOf('<body>')
      let end = report.indexOf('</body>')
      response.innerHTML = report.substring(start + 6, end)
      setState(data['score'])
      if (downloadButton !== undefined) {
        downloadButton.style.display = 'inline'
        downloadButton.data = data
      }
              
      clearErrorAnnotations();                
      if ('errors' in data) {
        for (let i = 0; i < data['errors'].length; i++) {
          let error = data['errors'][i]; 
          highlightLine(error['file'], error['line'], error['message']); }
      }
    }
    
    function prepareSubmit(url, prefix) {
      submitButton.addEventListener('click', function() {
        clearErrorAnnotations();        
        response.textContent = 'Submitting...'
        let params = {}
        let inputs = form.getElementsByTagName('input');
        for (let i = 0; i < inputs.length; i++) {
          let name = inputs[i].getAttribute('name')
          if (name !== null) 
            params[name] = inputs[i].getAttribute('value')
        }
        
        let files = form.getElementsByClassName('file');
        for (let i = 0; i < files.length; i++) {
          let allContent = "";
          let editorDivs = files[i].getElementsByClassName('editor');
          for (let k = 0; k < editorDivs.length; k++) {
            if (k > 0) allContent += "\n"
            allContent += ace.edit(editorDivs[k]).getValue();
          }
          let filename = files[i].getAttribute('name');
          params[filename] = allContent
        }
        
        submitButton.classList.add('hc-disabled')
        if (downloadButton !== undefined) downloadButton.style.display = 'none'

        // TODO: Do I need to do anything about CORS?
        // withCredentials???
        let xhr = new XMLHttpRequest()
        xhr.withCredentials = true
        xhr.timeout = 300000 // 5 minutes
        xhr.open('POST', url);    
        xhr.setRequestHeader('Content-Type', 'application/json');
        xhr.onload = function() {
          submitButton.classList.remove('hc-disabled');
          if (xhr.status === 200) 
            successfulSubmission(JSON.parse(xhr.responseText))
          else 
            response.innerHTML = 
              '<div>Error Status: ' + xhr.status + ' ' + xhr.statusText + '</div>\n' +
              '<div>Error Response: ' + xhr.responseText + '</div>\n';
        }
        xhr.send(JSON.stringify(params))
      })
    }
      
    // Start of initElement
    element.classList.add('vstdonthighlight')
    element.classList.add('vst-click')    
      
    initUI()
      
    horstmann_config.retrieve_state && horstmann_config.retrieve_state(element, restoreState);
  }

  // Start of event listener
  let elements = document.getElementsByClassName('horstmann_codecheck')  
  for (let index = 0; index < elements.length; index++) 
    initElement(elements[index],
                window.horstmann_codecheck.setup[index],
                'horstmann_codecheck' + (index + 1) + '-') 
});
