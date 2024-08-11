// Uses postData, createButton from util.js

window.horstmann_codecheck = {
  setup: [],
};

if (typeof ace !== 'undefined') { ace.config.set('themePath', 'script'); } 

window.addEventListener('load', async function () {
  'use strict';

  function createRearrange(fileName, setup) {
    'use strict';

    // Element-scoped variables
    const INDENT_STRING = '\u2002\u2002\u2002' // en space
    const left = document.createElement('div')
    const right = document.createElement('div')
    let drag = undefined   
    let indentWidth = undefined
    const codeMap = new Map()

    function getClientXY(e) {
      return e.touches ? {
        x: e.touches[0].clientX,
        y: e.touches[0].clientY
      } : {
        x: e.clientX,
        y: e.clientY
      }
    }

    function createTile(text) {
      let tileDiv = document.createElement('div')
      tileDiv.classList.add('tile')
      tileDiv.classList.add('hc-code')
      tileDiv.textContent = text
      return tileDiv
    }

    function setIndent(tileDiv, indent) {
      tileDiv.style.marginLeft = ((indent + 1) * indentWidth) + 'px'
      tileDiv.indent = indent
    }

    function shiftTile(tileDiv, droppedTileX) {      
      let leftX = left.getBoundingClientRect().left // TODO: Adjust for margin
      let indent = Math.max(0, Math.round((droppedTileX - leftX) / indentWidth - 1))
      setIndent(tileDiv, indent)
    }

    function codeOf(tile) {
      let code = codeMap.get(tile)
    return code === undefined ? tile.textContent : code   
    }
    
    function measureIndentWidth() {
      // Measure indent width
      let tileDiv = createTile()
      let span = document.createElement('span')
      span.innerHTML = INDENT_STRING
      tileDiv.appendChild(span)
      left.appendChild(tileDiv)
      indentWidth = Math.round(span.getBoundingClientRect().width)
      left.removeChild(tileDiv)

      
      if (indentWidth === 0) 
        setTimeout(measureIndentWidth, 2000)
      else {
        left.style.background = `repeating-linear-gradient( to right, transparent 0px ${indentWidth}px, #aeede9 ${indentWidth}px ${indentWidth + 1}px )`
        for (let i = 0; i < left.children.length; i++) {
          let tileDiv = left.children[i]
          setIndent(tileDiv, tileDiv.indent)
        }
        right.children[0].focus()
      }
    }

    function stickyBrace(tileDiv) {
      if (tileDiv.parentNode !== left) return
      if (!codeOf(tileDiv).endsWith('{')) return
      
      // Is a } available to the right? 
      let rightBrace = null
      for (let i = 0; rightBrace === null && i < right.children.length; i++)
        if (codeOf(right.children[i]) === '}') rightBrace = right.children[i]
      if (rightBrace === null) return

      // Is there a } tile below with matching indent? If so, return 
      let i = 0
      while (left.children[i] !== tileDiv) i++
      i++
      let done = false
      while (!done && i < left.children.length) {
        let tileDivBelow = left.children[i]
        if (tileDivBelow.indent === tileDiv.indent && codeOf(tileDivBelow).trim().startsWith('}')) return
        else if (tileDivBelow.indent < tileDiv.indent) done = true
        else i++
      }
      
      // Still here ... move the rightBrace 

      right.removeChild(rightBrace)
      left.insertBefore(rightBrace, tileDiv.nextSibling)      
      setIndent(rightBrace, tileDiv.indent)
    }

    function insertDroppedTileLeft(tileDiv, droppedTileX, droppedTileY) {
      const droppedTileCenterY = droppedTileY + tileDiv.clientHeight / 2
      
      let leftX = left.getBoundingClientRect().left // TODO: Adjust for margin
      let indent = Math.max(0, Math.round((droppedTileX - leftX) / indentWidth - 1))
      setIndent(tileDiv, indent)

      let done = false
      let fromRight = tileDiv.parentNode === right
      let i = 0
      tileDiv.parentNode.removeChild(tileDiv)
      while (i < left.children.length && !done) {
        let child = left.children[i]
        let bounds = child.getBoundingClientRect() 
        if (droppedTileCenterY < bounds.top + child.clientHeight) {
          left.insertBefore(tileDiv, child)
          done = true
        }
        else 
          i++
      }      
      if (!done) 
        left.appendChild(tileDiv)
      if (fromRight) stickyBrace(tileDiv)
      tileDiv.focus()
    }

    function insertDroppedTileRight(tileDiv) {
      tileDiv.parentNode.removeChild(tileDiv)
      right.appendChild(tileDiv)
      tileDiv.style.marginLeft = ''
      right.children[0].focus()
    }

    function makeTile(contents, isFixed) {
      let tileDiv = createTile()
      let text
      if (typeof contents === 'object') {
        text = contents.text
        if ('code' in contents) {
          codeMap.set(tileDiv, contents.code)
          tileDiv.classList.add('pseudo')            
        }   
      }
      else {
        text = contents
      }
      if (isFixed) {
        tileDiv.classList.add('fixed')
        tileDiv.setAttribute('draggable', false);
        tileDiv.tabIndex = -1
      if (typeof contents === 'object' && 'indent' in contents) {
          setIndent(tileDiv, contents.indent)   
      } else {
          let lines = text.split('\n')
          let minIndent = Number.MAX_SAFE_INTEGER
          for (const line of lines) {
            let indent = 0
            while (line[indent] === '\t') indent++
            minIndent = Math.min(minIndent, indent)
          }
          let strippedText = ''
          for (const line of lines) {
            if (strippedText !== '') strippedText += '\n'
            strippedText += line.substring(minIndent).replace('\t', '   ')
          }          
          tileDiv.textContent = strippedText
          setIndent(tileDiv, minIndent)
        }
      }
      else {
        tileDiv.textContent = text
        tileDiv.setAttribute('draggable', true);
        tileDiv.tabIndex = 0
        const mousedownListener = function(e) {
          if (tileDiv !== document.activeElement) {
            tileDiv.focus()
          }
          else if (Array.prototype.indexOf.call(left.children, tileDiv) >= 0) {
            shiftTile(tileDiv, getClientXY(e).x - indentWidth / 2)
            tileDiv.blur()
          }
          e.stopPropagation() // So that the left/right div doesn't get it
          // Don't call e.preventDefault(), or dragging no longer works
        }
        tileDiv.addEventListener('mousedown', mousedownListener)
        tileDiv.addEventListener('touchstart', mousedownListener)
        
        tileDiv.addEventListener('dragstart', function(e) {
          e.dataTransfer.effectAllowed = 'all';
          e.dataTransfer.setData('text/plain', tileDiv.textContent); // Firefox needs this 
          let bounds = tileDiv.getBoundingClientRect()
          let p = getClientXY(e)
          drag = {
            tile: tileDiv,
            x: p.x - bounds.left,
            y: p.y - bounds.top
          }
          
          if (typeof horstmann_common === 'object' && horstmann_common.iOS) { // iOS uselessly scales down the preview image
            drag.x = 32
            drag.y = 32
            // A 64 x 64 icon
            // SVG didn't work on iOS 12
            // A 400 x 40 rectangle got scaled down so the left side
            // was useless for positioning
            let img = document.createElement('img')
            img.src = 'data:image/png;base64,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'
            e.dataTransfer.setDragImage(img, 32, 32)
          }
        })            
        tileDiv.addEventListener('dragend', function(e) {
          drag = undefined
        })
        
        // TODO: keyCode deprecated https://stackoverflow.com/questions/35394937/keyboardevent-keycode-deprecated-what-does-this-mean-in-practice
        
        tileDiv.addEventListener('keydown', function(e) {
          if (e.keyCode === 37) { // left
            moveLeftRight(tileDiv, -1)
          } else if (e.keyCode === 39) { // right
            moveLeftRight(tileDiv, 1)
          } else if (e.keyCode === 38) { // up
            moveUpDown(tileDiv, -1)
          } else if (e.keyCode === 40) { // down
            moveUpDown(tileDiv, 1)
          } else {
            return true;
          }
          e.stopPropagation();
          e.preventDefault()
          return false; 
        })
      }
      tileDiv.addEventListener('keypress', function(e) {
        if (37 <= e.keyCode && e.keyCode <= 40) {
          e.stopPropagation();
          e.preventDefault()          
          return false;
        } else if (e.keyCode === 125) { // }
          stickyBrace(tileDiv)
          return true;
        }
      })
      tileDiv.addEventListener('keyup', function(e) {
        if (37 <= e.keyCode && e.keyCode <= 40) {
          e.stopPropagation();
          e.preventDefault()          
          return false;
        }
      })
      
      return tileDiv
    }

    function moveUpDown(tileDiv, dy) {
      let parent = tileDiv.parentNode
      if (parent === right) return
      if (dy === -1) {
        let sibling = tileDiv.previousSibling
        if (sibling != tileDiv.parentNode.firstChild)
          parent.insertBefore(tileDiv, sibling)
      }
      else {
        let sibling = tileDiv.nextSibling
        if (sibling) 
          parent.insertBefore(tileDiv, sibling.nextSibling)
      }
      tileDiv.focus()
    }

    function moveLeftRight(tileDiv, dx) {
      let parent = tileDiv.parentNode
      let indent = tileDiv.indent + dx

      if (parent === right) {
        left.appendChild(tileDiv)
        setIndent(tileDiv, 0)
      }
      else if (0 <= indent) {
        setIndent(tileDiv, indent)
      } else {
        right.appendChild(tileDiv)
        tileDiv.style.marginLeft = 0
      }
      tileDiv.focus()
    }  

    const mousedownListener = function(e) {
      let focusedElement = document.activeElement
      if (focusedElement.classList.contains('fixed')) return
      let tileOnLeft = [...left.children].indexOf(focusedElement) >= 0
      let tileOnRight = [...right.children].indexOf(focusedElement) >= 0
      if (!tileOnLeft && !tileOnRight) return
      if (e.target === left) {
        let p = getClientXY(e)
        let droppedTileX = p.x - indentWidth / 2
        let droppedTileY = p.y - indentWidth / 2

        insertDroppedTileLeft(focusedElement, droppedTileX, droppedTileY)
      }
      else if (tileOnLeft)
        insertDroppedTileRight(focusedElement)
    }

    function initialize() {  
      left.classList.add('left')
      right.classList.add('right')
      const both = document.createElement('div')
      both.appendChild(left)
      both.appendChild(right)

      measureIndentWidth()

      // Add event listeners
      left.addEventListener('mousedown', mousedownListener)
      left.addEventListener('touchstart', mousedownListener)
      right.addEventListener('mousedown', mousedownListener)
      right.addEventListener('touchstart', mousedownListener)
      
      left.addEventListener('dragenter', function(e) {
        if (drag === undefined) return // Some sort of foreign drop
        e.preventDefault()
        left.classList.add('dragover')
      })
      left.addEventListener('dragleave', function(e) {
        left.classList.remove('dragover')
      })      
      left.addEventListener('dragover', function(e) {
        e.preventDefault()
        e.dataTransfer.dropEffect = 'move'
      })
      left.addEventListener('drop', function(e) {
        left.classList.remove('dragover')
        if (drag === undefined) return // Some sort of foreign drop
        let tileDiv = drag.tile
        let p = getClientXY(e)
        let droppedTileX = p.x - drag.x
        let droppedTileY = p.y - drag.y
        drag = undefined        
        insertDroppedTileLeft(tileDiv, droppedTileX, droppedTileY)
        e.preventDefault()
      })

      right.addEventListener('dragenter', function(e) {
        e.preventDefault() 
        right.classList.add('dragover')
      })
      right.addEventListener('dragleave', function(e) {
        right.classList.remove('dragover')
      })
      right.addEventListener('dragover', function(e) {
        e.preventDefault() 
        e.dataTransfer.dropEffect = 'move'
      })
      right.addEventListener('drop', function(e) {
        right.classList.remove('dragover')
        if (drag === undefined) return // Some sort of foreign drop
        e.preventDefault() 
        let tileDiv = drag.tile
        drag = undefined
        insertDroppedTileRight(tileDiv)
      })
      if ('fixed' in setup)
        for (const fixed of setup.fixed)
          left.appendChild(makeTile(fixed, true)) 
      if ('tiles' in setup)
        for (const tile of setup.tiles)
          right.appendChild(makeTile(tile, false)) 
      both.classList.add('horstmann_rearrange')

      return both
    }

    function getState() {
      const leftTiles = []
      let group = []
      for (const tile of left.children) {
        if (tile.classList.contains('fixed')) {
          leftTiles.push(group)
          group = []
        }
        else {
          let state = { text: tile.textContent, indent: tile.indent }
          let code = codeMap.get(tile)
          if (code !== undefined) state.code = code  
          group.push(state) 
        } 
      }
      leftTiles.push(group)
      
      return {
        left: leftTiles,
        right: [...right.children].map(tile => {
            let code = codeMap.get(tile)
            if (code !== undefined) return { text: tile.textContent, code }  
            else return tile.textContent
        })  
      }    
    }

    function restoreState(state) {
      codeMap.clear()
      let i = 0
      let leftTiles = [...left.children]
      for (const tile of leftTiles) {
        if (tile.classList.contains('fixed')) {
          let group = state.left[i]
          i++
          for (let j = group.length - 1; j >= 0; j--) {
            const newTile = makeTile(group[j], false)
            left.insertBefore(newTile, tile)
            setIndent(newTile, tile.group[i].indent)
          }
        }
        else
          left.removeChild(tile)
      }
      for (const t of state.left[i]) {
        const newTile = makeTile(t, false) 
        left.appendChild(newTile)      
        setIndent(newTile, t.indent)
      }
      right.innerHTML = ''
      for (const t of state.right) {
        const newTile = makeTile(t, false) 
        right.appendChild(newTile)      
      }
    }

    function getText() {
      let content = ''
      for (const tile of left.children) {
      let code = codeMap.get(tile)
        if (tile.classList.contains('fixed') && code !== undefined) 
          content += code + '\n'
        else {
        if (code === undefined) code = tile.textContent 
          for (const line of code.split('\n'))
            content += '\t'.repeat(tile.indent) + line + '\n'
      }
      }
      return content
    }

    function errorAnnotation(lineNumber, message) {
      let l = 1
      for (const tile of left.children) {
        const lines = codeOf(tile).split('\n').length
        if (l <= lineNumber && lineNumber < l + lines) {
          tile.classList.add('hc-bad')
          tile.title = message        
        }
        l += lines
      }    
    }

    function clearErrorAnnotations() {
      for (const tile of left.children) {
        tile.classList.remove('hc-bad')
        tile.title = ''
      }
    }  
    
    return {
      initialize,
      getState,
      restoreState,
      getText,
      errorAnnotation,
      clearErrorAnnotations,
    }
  }

  // ..................................................................

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
      if (key && typeof key == 'object')
        key = key[editor.commands.platform];
      if (/backspace|delete/i.test(key))
        editor.commands.bindKey(key, 'null')
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
      editor.setTheme('ace/theme/kuroir');
      editor.setReadOnly(true);
      // At one time, the cursor was completely blocked, but it seems reasonable to allow copying the code
      // https://stackoverflow.com/questions/32806060/is-there-a-programmatic-way-to-hide-the-cursor-in-ace-editor
      //editor.renderer.$cursorLayer.element.style.display = 'none'
      // https://github.com/ajaxorg/ace/issues/266
      //editor.textInput.getElement().tabIndex = -1
    } else {
      editor.setTheme('ace/theme/chrome');
      
      let editorHandleTextFieldEvents = function (e) {
        if (e.keyCode === 37 || e.keyCode === 39) // left or right
          e.stopPropagation();
      }

      editor.textInput.getElement().addEventListener('keyup', editorHandleTextFieldEvents)
      editor.textInput.getElement().addEventListener('keydown', editorHandleTextFieldEvents)

      editor.on('focus', function() {
        setCommandEnabled(editor, 'indent', true)
        setCommandEnabled(editor, 'outdent', true)
      })

      editor.commands.addCommand({
        name: 'escape',
        bindKey: {win: 'Esc', mac: 'Esc'},
        exec: function() {
          setCommandEnabled(editor, 'indent', false)
          setCommandEnabled(editor, 'outdent', false)
        }
      });
    }
    let tas = editorDiv.getElementsByTagName('textarea')
    for (let i = 0; i < tas.length; i++) {
      tas[i].setAttribute('aria-label', 'Complete this code')
    }
  }        

  function createAceEditors(fileName, setup) {
    let editorsDiv = document.createElement('div')        
    function initialize() {
      let editorCount = 0;
      let update = function() {
        let totalLines = 0;
        for (const editorDiv of editorsDiv.children) {
          let editor = ace.edit(editorDiv)
          let editorSession = editor.getSession()
          editorSession.clearAnnotations()
          editorSession.setOption('firstLineNumber', totalLines + 1)
          let lines = editorSession.getDocument().getLength()
          editor.setOptions({
            minLines: lines,
            maxLines: lines
          })        
          editor.resize()
          totalLines += lines
        }
      }

      for (let i = 0; i < setup.length; i++) {
        let contentSegment = setup[i]
        if (contentSegment === null)
          continue
        
        let readonly = i % 2 !== 0; 
        let editorDiv = document.createElement('div')
        editorDiv.classList.add('editor')
        if (fileName === 'Input')
          editorDiv.classList.add('input')
        editorDiv.textContent = contentSegment.replace(/\r?\n$/, '')
        let editor = ace.edit(editorDiv)
        if (readonly)
          editorDiv.setAttribute('readonly', 'readonly')
        else
          editor.on('change', update)
        setupAceEditor(editorDiv, editor, fileName, readonly)        
        
        editorsDiv.appendChild(editorDiv)
      }
      update()
      
      return editorsDiv
    }

    function getText() {
      let content = ''
      for (const editorDiv of editorsDiv.children) {
        content += ace.edit(editorDiv).getValue() + '\n';
      }
      return content
    }

    function clearErrorAnnotations() { 
      for (const editorDiv of editorsDiv.children) 
        ace.edit(editorDiv).getSession().clearAnnotations()
    }
    
    function errorAnnotation(line, message) {
      let totalLines = 0
      for (const editorDiv of editorsDiv.children) {
        const editorSession = ace.edit(editorDiv).getSession();
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

    function restoreState(state) {
      let editableCount = 0
      for (const editorDiv of editorsDiv.children) {
        if (editorDiv.getAttribute('readonly') !== 'readonly') {
          let editor = ace.edit(editorDiv)
          editor.setValue(state[editableCount])
          editor.clearSelection()            
          editableCount++
        }
      }
    }

    function getState() {
      let editableCount = 0
      let state = []
      for (const editorDiv of editorsDiv.children) {
        if (editorDiv.getAttribute('readonly') !== 'readonly') {
          state.push(ace.edit(editorDiv).getValue())
          editableCount++
        }
      }
      return state
    }
    
    return {
      initialize,
      getState,
      restoreState,
      getText,
      errorAnnotation,
      clearErrorAnnotations,
    }      
  }

  // ..................................................................

  function initElement(element, setup, prefix) {    
    let form = undefined
    let response = undefined
    let submitButton = undefined
    let downloadButton = undefined
    let editors = new Map()

    function restoreState(dummy, state) { // TODO: Eliminate dummy
      if (state === null || state === undefined) return; // TODO Can it be null???
      let work = state.work
      if ('studentWork' in state) { // TODO: Legacy state
        work = {}
        const indexMapping = {}
        for (const fileElement of element.getElementsByClassName('file')) {
          const fileName = fileElement.getAttribute('name')
          indexMapping[fileName] = []
          work[fileName] = []
          let editableCount = 0
          let suffix = 0
          for (const editorDiv of fileElement.children[1].children) {
              suffix++
              if (editorDiv.getAttribute('readonly') !== 'readonly') {
                indexMapping[fileName][suffix] = editableCount
                editableCount++
              }
            }          
        }
        
        for (const entry of state.studentWork) {
          const fragmentName = entry.problemName
          let i = fragmentName.lastIndexOf('-')
          const fileName = fragmentName.substring(0, i)
          const fragmentIndex = parseInt(fragmentName.substring(i + 1))
          const editableIndex = indexMapping[fileName][fragmentIndex]
          if (editableIndex !== undefined)
            work[fileName][editableIndex] = entry.code
        }
      } 
      for (let fileName in work) 
        editors.get(fileName).restoreState(work[fileName])
      if (state.hasOwnProperty('scoreText')) {
        response.textContent = 'Score: ' + state.scoreText
      }
    }    

    function editorFor(fileName, fileSetup) {
      if ('tiles' in fileSetup)
        return createRearrange(fileName, fileSetup)
      else if ('editors' in fileSetup)
        return createAceEditors(fileName, fileSetup.editors)
      else
        return createAceEditors(fileName, fileSetup)
    }
    
    function appendRequiredFile(fileName, directoryPrefix) {
      let fileDiv = document.createElement('div')
      fileDiv.setAttribute('name', fileName)
      fileDiv.classList.add('file')
      let filenameDiv = document.createElement('div')
      filenameDiv.textContent = directoryPrefix + fileName
      filenameDiv.classList.add('codecheckFilename')
      fileDiv.appendChild(filenameDiv)
      
      let fileSetup = setup.requiredFiles[fileName];
      let editor = editorFor(fileName, fileSetup)
      editors.set(fileName, editor)
      fileDiv.appendChild(editor.initialize());     
      
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
        let text = setup.useFiles[fileName].replace(/\r?\n$/, '')
        editorDiv.textContent = text
        let editor = ace.edit(editorDiv)
        
        let fileObj = document.createElement('div')
        fileObj.classList.add('codecheckUseFile')
        let filenameDiv = document.createElement('div')
        filenameDiv.classList.add('codecheckFilename')
        filenameDiv.textContent = directoryPrefix + fileName
        fileObj.appendChild(filenameDiv)
        fileObj.appendChild(editorDiv)
        const MAX_LINES = 200
        const lines =  text.split(/\n/).length            
        editor.setOption('maxLines', Math.min(lines, MAX_LINES))
        setupAceEditor(editorDiv, editor, fileName, /*readonly*/ true)
        form.appendChild(fileObj)

        if (lines > MAX_LINES) {
          const viewButton = createButton('hc-command', _('Expand'), function() {
			if (editor.getOption('maxLines') > MAX_LINES) {
                editor.setOption('maxLines', MAX_LINES)
                editor.resize()               
                viewButton.innerHTML = _('Expand')
            }
            else {
                editor.setOption('maxLines', lines)
                editor.resize()          
                viewButton.innerHTML = _('Collapse')
            }
          })
          form.appendChild(viewButton)
        }
      }  
      
	  submitButton = createButton('hc-start', submitButtonLabel, async function() {
        response.textContent = 'Submitting...'
        let params = {}
        // Hidden inputs
        for (const input of form.getElementsByTagName('input')) {
          let name = input.getAttribute('name')
          if (name !== null) 
            params[name] = input.getAttribute('value')
        }

        for (const [filename, editor] of editors) {
          editor.clearErrorAnnotations()
          params[filename] = editor.getText()
        }
        
        submitButton.classList.add('hc-disabled')
        if (downloadButton !== undefined) downloadButton.style.display = 'none'
        try {
          const result = await postData(setup.url, params)
          successfulSubmission(result)
        } catch (e) {
          response.innerHTML = `<div>Error: ${e.message}</div>` 
		}
		submitButton.classList.remove('hc-disabled');
      })

      submitDiv.appendChild(submitButton);

      
      let resetButton = createButton('hc-start', _('Reset'), function() {
        restoreState(element, initialState)
        element.correct = 0;
        response.innerHTML = ''
        if (downloadButton !== undefined) downloadButton.style.display = 'none'
      })
      submitDiv.appendChild(resetButton);

      if ('download' in horstmann_config) {
        downloadButton = createButton('hc-start', _('Download'), () => {
          horstmann_config.download('data:application/octet-stream;base64,' + downloadButton.data.zip, downloadButton.data.metadata.ID + '.signed.zip', 'application/octet-stream') 
        })
        downloadButton.style.display = 'none'
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
      
      element.appendChild(form)
            
      let initialState = getState();
    }

    function getState() {
      const work = {}
      for (const [filename, editor] of editors) 
          work[filename] = editor.getState()
      return { work }
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
              
      if ('errors' in data) {
        for (const error of data.errors) {
          const editor = editors.get(error['file'])
          // TODO: Non-editable files are not in editors. Would be nice to annotate anyway 			
          if (editor !== undefined) editor.errorAnnotation(error['line'], error['message'])
        }
      }
    }
    
    // ..................................................................
    // Start of initElement
      
    initUI()
      
    horstmann_config.retrieve_state && horstmann_config.retrieve_state(element, restoreState);
  }

  // ..................................................................
 
  // Start of event listener
  let elements = document.getElementsByClassName('horstmann_codecheck')
  for (let index = 0; index < elements.length; index++) {
	const setup = window.horstmann_codecheck.setup[index]
	/*
	  Required properties: 
        repo
	    problem
	  Optional:
	    url
	    requiredFiles (if not present, obtained with fileData)
	    useFiles
	    order
	    prefix
	*/
	if ('requiredFiles' in setup)   
      initElement(elements[index], setup)
    else {
	  const origin = new URL(window.location.href).origin
	  const response = await fetch(`${origin}/fileData?repo=${setup.repo}&problem=${setup.problem}`)
      const data = await response.json()
      initElement(elements[index], { url: `${origin}/checkNJS`, ...data, ...setup })
    }	                
  }
});
