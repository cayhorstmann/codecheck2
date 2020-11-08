// https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API/Using_Fetch
async function postData(url = '', data = {}) {
  // Default options are marked with *
  const response = await fetch(url, {
    method: 'POST', // *GET, POST, PUT, DELETE, etc.
    mode: 'cors', // no-cors, *cors, same-origin
    cache: 'no-cache', // *default, no-cache, reload, force-cache, only-if-cached
    credentials: 'include', // include, *same-origin, omit
    headers: {
      'Content-Type': 'application/json'
      // 'Content-Type': 'application/x-www-form-urlencoded',
    },
    redirect: 'follow', // manual, *follow, error
    referrerPolicy: 'no-referrer', // no-referrer, *no-referrer-when-downgrade, origin, origin-when-cross-origin, same-origin, strict-origin, strict-origin-when-cross-origin, unsafe-url
    body: JSON.stringify(data) // body data type must match "Content-Type" header
  });
  if (response.ok)
    return await response.json() // parses JSON response into native JavaScript objects
  else 
    throw new Error(`${response.status} ${response.statusText}`)
}

function createButton(clazz, label, action) {
  let button = document.createElement('span')
  button.classList.add('hc-button')
  button.classList.add(clazz)
  button.innerHTML = label
  button.tabIndex = 0 
  button.addEventListener('click', action)
  button.addEventListener('keydown', function(e) {
    if (e.keyCode === 32) {
      e.stopPropagation();
      e.preventDefault();
      action()
    } else if (e.keyCode === 37 || e.keyCode === 39) {
      e.stopPropagation();
    }
  });      
  return button    
}

function percent(score) {
  return Math.round(score * 100.0).toFixed(0) + '%'
}

function append(parent, type, content) {
  const child = document.createElement(type)
  parent.appendChild(child)
  if (typeof content === 'string')
    child.textContent = content
  else if (content !== undefined)
    child.appendChild(content)
  return child
}


