/* Use for iframe communication */
// https://developer.mozilla.org/en-US/docs/Web/API/Window/postMessage   
function receiveMessage(event) {
    var origin = event.origin || event.originalEvent.origin;
    // For Chrome, the origin property is in the event.originalEvent object.
    // TODO: Filter origin?
    if (event.data.query === 'docHeight') {
        var body = document.body
        var html = document.documentElement;
        var fudge = 50;
        var height = Math.max( body.scrollHeight, body.offsetHeight,
                html.clientHeight, html.scrollHeight, html.offsetHeight ) + fudge;
        event.source.postMessage({docHeight: height, request: event.data }, '*' );
        return;
    }
    // default action is to report score
    
    var repo = $('input[name=repo]').attr('value');
    var problem = $('input[name=problem]').attr('value');
    var scoreText = $('.codecheck-submit-response').data('score');
    var correct = 0;
    var maxscore = 1; // default maxscore. not 0 to avoid divide by zero
    if (scoreText !== undefined && scoreText !== '0' && scoreText.length > 0) {
        correct = scoreText.split('/')[0];
        maxscore = scoreText.split('/')[1];
    }
    var response = {correct: correct, errors: 0, maxscore: maxscore, repo: repo, problem: problem, request: event.data }
    event.source.postMessage(response, '*');
}

window.addEventListener("message", receiveMessage, false);