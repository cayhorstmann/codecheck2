const fs = require('fs')
const http = require('http')
const formidable = require('formidable')
const child_process = require('child_process')

const server = http.createServer((req, res) => {
  if (req.url === '/api/health') {
    child_process.exec('/bin/df / ; /usr/bin/free -h', function(error, stdout, stderr) {
      /*
File system                 1K-blocchi     Usati Disponib. Uso% Montato su
/dev/mapper/ubuntu--vg-root  958139552 445501600 463897396  49% /
              total        used        free      shared  buff/cache   available
Mem:           15Gi       9.6Gi       644Mi       1.0Gi       5.1Gi       4.5Gi
Swap:          15Gi       8.9Gi       7.1Gi

      */
      const regexp = /.*\s+(?<diskfull>[0-9.]+%).*Mem:(?:\s+[0-9.]+(?:B|Ki|Mi|Gi|Ti|Pi)){4}\s+(?<available>[0-9.]+(?:B|Ki|Mi|Gi|Ti|Pi))\s+.*/s
      const result = regexp.exec(stdout).groups      
      res.end(JSON.stringify(result) + '\n')
    })
    res.writeHead(200, { 'Content-Type': 'application/json' })
    return
  }
  if (req.url === '/api/upload' && req.method.toLowerCase() === 'post') {
    // parse a file upload
    const form = formidable({ multiples: false })
    form.parse(req, (err, fields, files) => {
      try {
        const path = files.job.path
        child_process.exec(`/opt/codecheck/check ${path}`, function(error, stdout, stderr) {
          try {
            if (error !== null) console.log({error})
            else if (stderr.length > 0) console.log({stderr})
            const lines = stdout.split("\n")
            for (let i = 0; i < lines.length - 2; i++)
              console.log(lines[i])
            const out = lines[lines.length - 2]  
            res.writeHead(200, {'Content-disposition': 'attachment filename=out.zip"', 'Content-type': 'application/zip'})
            
            const stream = fs.createReadStream(out)
            stream.on('open', function () {
              stream.pipe(res)
            })

            stream.on('end', function() {
              fs.unlink(out, function() { })
            })

            stream.on('error', function(e) {
              fs.unlink(out, function() { })
              console.log(e)
              res.statusCode = 500
              res.write(e.message)
              res.end()
            })
          } catch (e) {
            console.log(e)
            res.statusCode = 500
            res.write(e.message)
            res.end()
          }
        })
      } catch (e) {
        console.log(e)
        res.statusCode = 500
        res.write(e.message)
        res.end()
      }
    })

    return
  }

  // show a file upload form
  res.writeHead(200, { 'Content-Type': 'text/html' })
  res.end(`
    <form action="/api/upload" enctype="multipart/form-data" method="post">
      <div>File: <input type="file" name="job"/></div>
      <input type="submit" value="Upload" />
    </form>
  `)
})
// TODO env.PORT?
server.listen(8080, () => {
  console.log('Server listening on http://localhost:8080/ ...')
})
