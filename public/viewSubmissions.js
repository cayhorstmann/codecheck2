/* 
   Willmaster Table Sort
   Version 1.1
   August 17, 2016
   Updated GetDateSortingKey() to correctly sort two-digit months and days numbers with leading 0.
   Version 1.0, July 3, 2011

   Will Bontrager
   https://www.willmaster.com/
   Copyright 2011,2016 Will Bontrager Software, LLC

   This software is provided "AS IS," without 
   any warranty of any kind, without even any 
   implied warranty such as merchantability 
   or fitness for a particular purpose.
   Will Bontrager Software, LLC grants 
   you a royalty free license to use or 
   modify this software provided this 
   notice appears on all copies. 
*/
var tableIDValue = 'submissions';
var tableLastSortedColumn = -1;

function sort_table() {
  var sortColumn = parseInt(arguments[0]);
  var type = arguments.length > 1 ? arguments[1] : 'T';
  var dateAndTimeFormat = arguments.length > 2 ? arguments[2] : '';
  var table = document.getElementById(tableIDValue);
  var tbody = table.getElementsByTagName('tbody')[0];
  var rows = tbody.getElementsByTagName('tr');
  var arrayOfRows = new Array();
  type = type.toUpperCase();
  dateAndTimeFormat = dateAndTimeFormat.toLowerCase();


  // Add the appropriate class to the sorted column header
  var headers = table.querySelectorAll('th');
  headers.forEach(function (header, index) {
    if (index !== sortColumn) {
      header.className = 'unsorted';
    } else {
    }
  });
  var sortedHeader = headers[sortColumn];
  if (sortedHeader.className == 'unsorted') {
    sortedHeader.className = 'ascending'
  }
  else if (sortedHeader.className == 'ascending') {
    sortedHeader.className = 'descending'
  }
  else {
    sortedHeader.className = 'ascending'
  }

  for (var i = 0, len = rows.length; i < len; i++) {
    arrayOfRows[i] = new Object();
    arrayOfRows[i].oldIndex = i;
    var celltext = rows[i].getElementsByTagName('td')[sortColumn].innerHTML.replace(/[^>]*>/g, '');

    if (type == 'D') {
      // Access the anchor tag within the cell
      var anchor =
        rows[i].getElementsByTagName('td')[sortColumn].getElementsByTagName('a')[0];
      var celltext = anchor ? anchor.textContent : ''; // Get the text content of the anchor
      arrayOfRows[i].value = get_date_and_time_sorting_key(dateAndTimeFormat, celltext);
    } else {
      var re = type == 'N' ? /[^\.\-\+\d]/g : /[^a-zA-Z0-9]/g;
      arrayOfRows[i].value = celltext
        .replace(re, '')
        .substr(0, 25)
        .toLowerCase();
    }
  }

  if (sortColumn == tableLastSortedColumn) {
    arrayOfRows.reverse();
  } else {
    tableLastSortedColumn = sortColumn;
    switch (type) {
      case 'N':
        arrayOfRows.sort(compare_row_of_numbers);
        break;
      case 'D':
        arrayOfRows.sort(compare_row_of_numbers);
        break;
      case 'S':
          // Checks if the first character of the first value is a number, and sorts rows accordingly.
          // If it's a number, sorts numerically; otherwise, sorts using text comparison.
        if (isNaN(arrayOfRows[0].value[0]))  {
          arrayOfRows.sort(compare_row_of_text);
        }
        else {
          arrayOfRows.sort(compare_row_of_numbers);
          break;
        }
      default:
        arrayOfRows.sort(compare_row_of_text);
    }
  }

  var newtableBody = document.createElement('tbody');
  for (var i = 0, len = arrayOfRows.length; i < len; i++) {
    newtableBody.appendChild(rows[arrayOfRows[i].oldIndex].cloneNode(true)
    );
  }
  table.replaceChild(newtableBody, tbody);
}

function compare_row_of_text(a, b) {  
  var aval = a.value;
  var bval = b.value;
  return aval == bval ? 0 : aval > bval ? 1 : -1;
}

function compare_row_of_numbers(a, b) {
  var aval = /\d/.test(a.value) ? parseFloat(a.value) : 0;
  var bval = /\d/.test(b.value) ? parseFloat(b.value) : 0;
  return aval == bval ? 0 : aval > bval ? 1 : -1;
}

function get_date_and_time_sorting_key(format, text) {
  if (format.length < 1) {
    return '';
  }
  format = format.toLowerCase();
  text = text.toLowerCase();
  text = text.replace(/^[^a-z0-9]*/, '');
  text = text.replace(/[^a-z0-9]*$/, '');
  if (text.length < 1) {
    return '';
  }
  text = text.replace(/[^a-z0-9]+/g, ',');
  var date = text.split(',');
  if (date.length < 7) {
    return '';
  }
  var d = 0,
    m = 0,
    y = 0;
  for (var i = 0; i < 7; i++) {
    var ts = format.substr(i, 1);
    if (ts == 'd') {
      d = date[i];
    } else if (ts == 'm') {
      m = date[i];
    } else if (ts == 'y') {
      y = date[i];
    } else if (ts == 'h') {
      h = date[i];
    } else if (ts == 't') {
      t = date[i];
    } else if (ts == 's') {
      s = date[i];
    } else if (ts == 'p') {
      p = date[i];
    }
  }
  d = d.replace(/^0/, '');
  if (d < 10) {
    d = '0' + d;
  }
  m = m.replace(/^0/, '');
  if (m < 10) {
    m = '0' + m;
  }
  y = parseInt(y);
  if (y < 100) {
    y = parseInt(y) + 2000;
  }
  if (p == 'pm') {
    h = parseInt(h) + 12;
  }
  if (h < 10) {
    h = '0' + h;
  }
  return ( '' + String(y) + '' + String(m) + '' + String(d) + '' + String(h) + '' + String(t) + '' + String(s));
}

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
    
    const body = document.querySelector('body')
    append(body, 'h1', 'Assignment Submissions')
    const buttonDiv = append(body, 'div')
    if (Object.keys(submissionData).length === 0) {
      append(body, 'p', 'No submissions yet')
    } else {
      append(body, 'h2', 'Submissions')
      buttonDiv.appendChild(createButton('hc-command', 'Download CSV', () => download_table_as_csv('submissions')))
      const jsonDownloadButton = document.createElement('a')
      jsonDownloadButton.classList.add('hc-button', 'hc-command')
      jsonDownloadButton.setAttribute('href', allSubmissionsURL)
      jsonDownloadButton.setAttribute('target', '_blank')
      jsonDownloadButton.textContent = 'Download JSON'
      buttonDiv.appendChild(jsonDownloadButton)
      const table = append(body, 'table')
      table.id = 'submissions'
      let tr = append(table, 'tr')

    // Creating headers
    const headers = [
      {
        text: 'Your Student Info',
        href: 'javascript:sort_table(0,"S");',
      },
      { text: 'Opaque ID', href: 'javascript:sort_table(1,"T");' },
      { text: 'Score', href: 'javascript:sort_table(2,"N");' },
      {
        text: 'Submitted At',
        href: 'javascript:sort_table(3,"D", "mdyhtsp");',
      },
    ];

    for (const header of headers) {
      const th = append(tr, 'th');
      th.setAttribute('title', 'Click to sort')
      if (header.text === 'Your Student Info') {
        const a = document.createElement('a');
        a.href = header.href;
        a.textContent = header.text;
        th.appendChild(a);
        th.style.display = 'none';
      } else {
        const a = document.createElement('a');
        a.href = header.href;
        a.textContent = header.text;
        th.appendChild(a);
      }
    }

    // Create tbody
    const tbody = document.createElement('tbody');
    table.appendChild(tbody);

    // Iterating through submissionData
    for (const submission of submissionData) {
      tr = append(tbody, 'tr');
      append(tr, 'td').style.display = 'none';
      append(tr, 'td', submission.opaqueID).classList.add('ccid');
      append(tr, 'td', percent(submission.score));
      const a = document.createElement('a');
      a.href = submission.viewURL;
      a.target = '_blank';
      a.textContent = new Date(submission.submittedAt).toLocaleString();
      append(tr, 'td', a);
    }

    const rosterDiv = append(body, 'div');
    append(rosterDiv, 'h2', 'Roster information');
    append(rosterDiv, 'p', 'Enter lines with CodeCheck student IDs and your corresponding student IDs/names here');
    const rosterTextArea = append(rosterDiv, 'textarea');
    rosterTextArea.rows = submissionData.length;
    rosterTextArea.cols = 80;
    rosterDiv.appendChild(createButton('hc-command', 'Add to table', () => {
        for (line of rosterTextArea.value.split('\n').map((s) => s.trim()).filter((s) => s != '')) {
          let ccid = line.split(/\s+/)[0];
          let info = line.substring(ccid.length).trim();
          for (const row of table.querySelectorAll('tr')) {
            row.children[0].style.display = '';
            if (row.children[1].textContent === ccid)
              row.children[0].textContent = info;
          }
        }
      })
    );
  }
})