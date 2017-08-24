function receiveMessage(event) {
    const origin = event.origin || event.originalEvent.origin;
    // For Chrome, the origin property is in the event.originalEvent object.
    // TODO: Filter origin?
    if (event.data.query === 'docHeight') {
        const body = document.body
        const html = document.documentElement;
        const fudge = 50;
        const height = Math.max( body.scrollHeight, body.offsetHeight,
                html.clientHeight, html.scrollHeight, html.offsetHeight ) + fudge;
        event.source.postMessage({docHeight: height, request: event.data}, '*' );
        return;
    } else if (event.data.query === 'restoreState') {
        const studentWork = event.data.state;

        for (let i = 0; i < studentWork.length; i++) {
            const problemName = studentWork[i].problemName;
            const studentCode = studentWork[i].code;

            // Need to get the textarea with the given id, then find the ace editor from there
            const editorDiv = document.getElementById(problemName).parentElement.getElementsByClassName('ace_editor')[0];
            const editor = ace.edit(editorDiv);
            editor.setValue(studentCode);
        }

        return;
    } else if (event.data.query === 'getContent') {
        const problems = document.querySelectorAll('body > form');

        let studentWork = [];
        for (let i = 0; i < problems.length; i++) {
            const problem = problems[i];

            const editorDiv = problem.getElementsByClassName('ace_editor')[0];
            const problemName = editorDiv.parentElement.getElementsByTagName('textarea')[0].id;
            const editor = ace.edit(editorDiv);

            studentWork.push({problemName: problemName, code: editor.getValue()});
        }
    
        // SUNITA SCORE CODE
        let repo = $('input[name=repo]').attr('value');
        var problem = $('input[name=problem]').attr('value');
        var scoreText = $('.codecheck-submit-response').data('score');
        var correct = 0;
        var maxscore = 1; // default maxscore. not 0 to avoid divide by zero
        if (scoreText !== undefined && scoreText !== '0' && scoreText.length > 0) {
            correct = scoreText.split('/')[0];
            maxscore = scoreText.split('/')[1];
        }
        var score = {correct: correct, errors: 0, maxscore: maxscore, repo: repo, problem: problem};
        // END SUNITA SCORE CODE

        const response = { request: event.data, score: score, state: studentWork };

        event.source.postMessage(response, event.origin);        
        return;
    }
    // default action is to report score
}

window.addEventListener("message", receiveMessage, false);