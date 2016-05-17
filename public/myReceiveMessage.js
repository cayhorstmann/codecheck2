/* Use for iframe communication */
// https://developer.mozilla.org/en-US/docs/Web/API/Window/postMessage   
function receiveMessage(event) {
    var origin = event.origin || event.originalEvent.origin;
    // For Chrome, the origin property is in the event.originalEvent object.
    // TODO: Filter origin?

    var repo = $('input[name=repo]').attr('value');
    var problem = $('input[name=problem]').attr('value');
    var scoreText = $('p.score').last().text();
    var correct = 0;
    var maxscore = 1; // default maxscore. not 0 to avoid divide by zero
    if (scoreText !== '0' && scoreText.length > 0) {
        correct = scoreText.split('/')[0];
        maxscore = scoreText.split('/')[1];
    }
    var response = {correct: correct, errors: 0, maxscore: maxscore, repo: repo, problem: problem}
    event.source.postMessage(response, '*');
}

window.addEventListener("message", receiveMessage, false);
