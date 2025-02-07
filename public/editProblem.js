window.addEventListener('DOMContentLoaded', () => {

let i = 1
let done = false	
while (!done) { 
  const deleteButton = document.getElementById('delete' + i)
  if (deleteButton == null) 
	done = true
  else {
    deleteButton.addEventListener('click',
	    function() {
	      document.getElementById('filename' + i).setAttribute('value', '')
	      document.getElementById('contents' + i).innerHTML = ''
	      document.getElementById('item' + i).style.display = 'none'
	    })
	  i++
	}
}
let fileIndex = i
document.getElementById('addfile').addEventListener('click',
  function() {
    let fileDiv = document.createElement('div')
    fileDiv.setAttribute('id', 'item' + fileIndex)
    fileDiv.innerHTML = '<p>File name: <input id="filename' + fileIndex + '" name="filename' + fileIndex 
            + '" size="25" type="text"/> <button id="delete' + fileIndex 
            +'" type="button">Delete</button></p><p><textarea id="contents' + fileIndex + '" name="contents' + fileIndex 
            + '" rows="24" cols="80"/></textarea></p>'
    let addFile = document.getElementById('addfilecontainer')
    addFile.parentNode.insertBefore(fileDiv, addFile)
    
    document.getElementById('delete' + fileIndex).addEventListener('click',
      function() {
        document.getElementById('filename' + fileIndex).setAttribute('value', '')
        document.getElementById('contents' + fileIndex).innerHTML = ''
        document.getElementById('item' + fileIndex).style.display = 'none'
    })
	fileIndex++
})

document.getElementById('upload').disabled = document.getElementById('file').files.length === 0 

document.getElementById('file').addEventListener('change', function() {
    document.getElementById('upload').disabled = document.getElementById('file').files.length === 0
})

})