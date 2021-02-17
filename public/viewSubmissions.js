    /*
     * TODO: Sort table
     */
window.addEventListener('DOMContentLoaded', () => {
// https://stackoverflow.com/questions/15547198/export-html-table-to-csv
  function download_table_as_csv(table_id) {
      // Select rows from table_id
      var rows = document.querySelectorAll('table#' + table_id + ' tr');
      // Construct csv
      var csv = [];
      for (var i = 0; i < rows.length; i++) {
          var row = [], cols = rows[i].querySelectorAll('td, th');
          for (var j = 0; j < cols.length; j++) {
              // Clean innertext to remove multiple spaces and jumpline (break csv)
              var data = cols[j].innerText.replace(/(\r\n|\n|\r)/gm, '').replace(/(\s\s)/gm, ' ')
              // Escape double-quote with double-double-quote (see https://stackoverflow.com/questions/17808511/properly-escape-a-double-quote-in-csv)
              data = data.replace(/"/g, '""');
              // Push escaped string
              row.push('"' + data + '"');
          }
          csv.push(row.join(';'));
      }
      var csv_string = csv.join('\n');
      // Download it
      var filename = 'export_' + table_id + '_' + new Date().toLocaleDateString() + '.csv';
      var link = document.createElement('a');
      link.style.display = 'none';
      link.setAttribute('target', '_blank');
      link.setAttribute('href', 'data:text/csv;charset=utf-8,' + encodeURIComponent(csv_string));
      link.setAttribute('download', filename);
      document.body.appendChild(link);
      link.click();
      document.body.removeChild(link);
  }
  
  function sorted(a) {
    return [...a].sort()
  }
  
  /*
  TODO: Sort table by clicking on column...
  */  
  
  const body = document.querySelector('body')
  append(body, 'h1', 'Assignment Submissions')
  const buttonDiv = append(body, 'div')
  if (Object.keys(submissionData).length === 0) {
    append(body, 'p', 'No submissions yet')
  } else {
    append(body, 'h2', 'Submissions')
    buttonDiv.append(createButton('hc-command', 'Download CSV', () => download_table_as_csv('submissions')))
    const jsonDownloadButton = document.createElement('a')
    jsonDownloadButton.classList.add('hc-command')
    jsonDownloadButton.setAttribute('href', '/lti/allSubmissions?resourceID=' + resourceID)
    jsonDownloadButton.setAttribute('target', '_blank')
    jsonDownloadButton.textContent = 'Download JSON'
    buttonDiv.append(jsonDownloadButton)
    const table = append(body, 'table')
    table.id = 'submissions'
    let tr = append(table, 'tr')
    append(tr, 'th', 'Your Student Info').style.display = 'none'
    append(tr, 'th', 'Opaque ID')
    append(tr, 'th', 'Score')
    append(tr, 'th', 'Submitted At')
    for (const submission of submissionData) {
      tr = append(table, 'tr')
      append(tr, 'td').style.display = 'none'     
      append(tr, 'td', submission.opaqueID).classList.add('ccid')
      append(tr, 'td', percent(submission.score))
      const a = document.createElement('a')
      a.href = submission.viewURL
      a.target = '_blank'
      a.textContent = new Date(submission.submittedAt).toLocaleString()
      append(tr, 'td', a)
    }
    const rosterDiv = append(body, 'div')
    append(rosterDiv, 'h2', 'Roster information')
    append(rosterDiv, 'p', 'Enter lines with CodeCheck student IDs and your corresponding student IDs/names here')
    const rosterTextArea = append(rosterDiv, 'textarea')
    rosterTextArea.rows = submissionData.length
    rosterTextArea.cols = 80
    rosterDiv.appendChild(createButton('hc-command', 'Add to Table', () => {
      for (line of rosterTextArea.value.split("\n").map(s => s.trim()).filter(s => s != '')) {
        let ccid = line.split(/\s+/)[0]
        let info = line.substring(ccid.length).trim()
        for (const row of table.querySelectorAll('tr')) {
          row.children[0].style.display = ''
          if (row.children[1].textContent === ccid)
            row.children[0].textContent = info
        } 
      }               
    }))  
  }  
})