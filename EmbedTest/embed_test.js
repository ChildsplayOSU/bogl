//
// Embedded BoGL Editor JS
//

(function() {

  let commandInput = [];
  function setCommandInput(sci) {
    commandInput = sci;
  }

  // used to track whether we are actively reading input to complete an expression or not
  let inputState = false;
  function setInputState(sis) {
    inputState = sis;
  }

  // used to track whether or not ':t ' was used to request printing of the type as well as the output
  let reportType = false;
  function setReportType(srt) {
    reportType = srt;
  }


  // Build board from JSON
  function get_board(board) {
      let res = "";
      for (let i = 0; i < board.length; i++) {
          for (let j = 0; j < board[i].length; j++) {
              if (j) {
                  res += "\t";
              }
              res += board[i][j][1]["value"];
          }
          res += "<br/>";
      }
      return res;
  }

  // We support nested tuples, so recursion is our friend
  function get_tuple(tuple) {
      let res = "(";
      for (let i = 0; i < tuple.length; i++) {
          if (i > 0) {
              res += ",";
          }
          if (tuple[i]["type"] === "Tuple") {
              res += get_tuple(tuple[i]["value"]);
          }
          else if (tuple[i]["type"] === "Board") {
              res += "<br/><br/>";
              res += get_board(tuple[i]["value"]);
              res += "<br/>";
          }
          else
          {
              res += tuple[i]["value"].toString();
          }
      }
      res += ")";
      return res;
  }

  // Used to stop handling input
  // Used after input has been accepted to full
  // or 'clear' has been typed
  function exitInputHandling() {
    setCommandInput([]);
    setInputState(false);
    command = "";
  }

  // Used to parse response from back-end server
  function parse_response(responses) {
      let latest = responses[responses.length-1];
      let res = "";
      let switch_mode = "";
      let boards = "";

      // Check if inputState switched
      switch (latest["tag"]) {
          case "SpielPrompt": {
              if (inputState === false) {
                  switch_mode = "<br/> 🤖 BoGL Says: Enter input, or \"clear\" to stop. <br/>";
                  inputState = true;
              }
              latest["contents"].forEach(b => boards = boards + get_board(b["value"]) + "<br/>");
              return boards + switch_mode;
          }
          case "SpielTypeError": {
              res = latest["contents"]["message"];
              return res;
          }
          case "SpielParseError": {
              console.log("Language (Parse) Error Found: "+latest["tag"]);
              //res = latest["contents"]["message"];
              // latest["contents"][0] = LINE NUM
              // latest["contents"][1] = COLUMN NUM
              // latest["contents"][2] = FILE NAME
              // latest["contents"][3] = MESSAGE
              // extract message from 4th item
              res = "Language Error: "+latest["contents"][3];
              return res;
          }
          case "SpielRuntimeError": {
            console.log("Runtime error encountered: "+ latest["contents"]);
            res = "Runtime Error: "+latest["contents"];
            if(inputState === true) {
              // flush cmd buffer so a retry will work,
              commandInput = [];
              res += "<br/>You entered an expression of incorrect type. Please enter an expression of the correct type.";
            }
            return res;
          }
          case "SpielTypeHole": {
              res = latest["contents"]["message"];
              return res;
          }
          default: {
              // Else it's a SpielValue
              if (inputState === true) {
                  switch_mode = "<br/><br/> 🤖 BoGL Says: Done reading input. <br/>";
                  exitInputHandling();
                  inputState = false;
              }
              break;
          }
      }


      // Parse response type
      switch (latest["contents"][1]["type"]) {
          case "Board": {
              res = get_board(latest["contents"][1]["value"]);
              break;
          }
          case "Tuple": {
              res = get_tuple(latest["contents"][1]["value"]);
              break;
          }
          default: {
              res = latest["contents"][1]["value"];
              break;
          }
      }

      // uppercase Javascript boolean values to match True/False in BoGL
      if(typeof res === "boolean" && res) {
        res = "True";
      } else if (typeof res === "boolean" && !res) {
        res = "False";
      }


      if(reportType) {
        // report this type, if marked with ':t ' from the cmd line
        let typ = latest["contents"][1]["type"];
        // get first letter in lowercase
        let fl = typ[0].toLowerCase();
        res += " is";

        if(fl === 'a' || fl === 'e' || fl === 'i' || fl === 'o' || fl === 'u') {
          // vowel sub y, use 'an'
          res += " an ";

        } else {
          // consonant
          res += " a ";

        }
        res += typ;

        // clear reporting
        setReportType(false);
        reportType = false;

      }


      latest["contents"][0].forEach(b => extendBoardsString(b, res, boards));
      return boards + res + switch_mode;
  }

  // an expression like place(1,b,(1,1)) evaluates to a board and it also adds the same board to
  // the print buffer. This makes sure we do not print duplicate boards.
  // an expression could modify two boards to be identical, in that case it may be confusing to
  // only print one board, but that is a much less common use case that the first one.
  function extendBoardsString(board, value, boards) {
      var boardStr = get_board(board["value"]);
      if (boardStr !== value) boards = boards + boardStr + value + "<br/>";
  }


  // Pushes item to command's input
  function input(next) {
      commandInput.push({"input":next});
      return;
  }

  // Clear input and state
  function clear() {
      exitInputHandling();
      return " 🤖 BoGL Says: Ok, skipping input. ";
  }

  function updateResults(resultElm, content) {
    resultElm.innerHTML += "> " + content + "<br/><br/>";
    // force scroll to new results, if any
    resultElm.scrollTop = resultElm.scrollHeight;
    resultElm.parentElement.scrollTop = resultElm.parentElement.scrollHeight;
  }

  window.onload = (function() {

    let lastCmd = "";
    let boglCode= document.getElementsByClassName("bogl-code");

    // clean up all code with proper html linebreaks
    for(let x = 0; x < boglCode.length; x++) {
      boglCode[x].innerHTML = boglCode[x].innerHTML.replaceAll(/\n/gi, '<br/>');
    }

    let results = document.getElementsByClassName("bogl-repl-result");
    let inputs  = document.getElementsByClassName("bogl-repl-run");
    for(let x = 0; x < inputs.length; x++) {
      inputs[x].addEventListener("keydown", (function(e) {
        if(e.keyCode != 13) {
          return;
        }

        // prevent buble
        e.preventDefault();

        let code = boglCode[x].innerHTML;
        // replace <br> with linebreaks
        code = code.replaceAll(/<div><br><\/div>/gi, '\n');
        // replace <div> and </div> with nothing!
        code = code.replaceAll(/(?:<div>)|(?:<\/div>)/gi, '\n');
        // cleanup leftover <br> tags
        code = code.replaceAll(/<br>/gi, '\n');

        let respStatus = 0;

        let cmd = inputs[x].value;
        inputs[x].value = "";

        if(cmd.replaceAll(/\s/gi, '') === "") {
          // nothing to run
          return;
        }

        // trim whitespace
        cmd = cmd.replace(/^\s+/, '').replace(/\s+$/, '');

        // speak haskell and colors change (this can be removed if it's annoying...)
        if(cmd.match(/haskell/i)) {
          boglCode[x].parentElement.className+= " haskell";
          updateResults(results[x], "The language that BoGL was inspired and built from, <a href=\"https://www.haskell.org/\" target=\"_blank\">https://haskell.org/</a>");
          return;

        } else if(cmd.match(/bogl/i)) {
          boglCode[x].parentElement.className= "bogl-embed-editor";
          updateResults(results[x], "BoGL");
          return;

        } else if(cmd.match(/help/i)) {
          updateResults(results[x], "Help<br/><br/>\
          This is a tiny version of the BoGL editor.<br/><br/>\
          - <a href=\"https://bogl.engr.oregonstate.edu/\" target=\"_blank\">Full BoGL Editor</a><br/>\
          - <a href=\"https://the-code-in-sheep-s-clothing.github.io/Spiel-Lang/\" target=\"_blank\">Website</a><br/>\
          - <a href=\"https://the-code-in-sheep-s-clothing.github.io/Spiel-Lang/Tutorials/All.md\" target=\"_blank\">Tutorials</a>");
          return;

        }

        if (inputState) {
            // put it into 'input' instead
            input(cmd);
            cmd = lastCmd;

        } else {
          lastCmd = cmd;

        }

        // check to ':t ' to request an expression result type
        let regex = /^:t\s+/;
        if(cmd.match(regex)) {
          // pull off the prefaced type instruction, but note that we would like to report a type once done
          cmd = cmd.replace(regex,'');
          setReportType(true);
          reportType = true;

        } else {
          // do not report type
          setReportType(false);
          reportType = false;

        }

        if(cmd == "clear") {
          updateResults(results[x], cmd + "<br/><br/>" + clear());
          return;

        }

        fetch(
            //"http://localhost:3000/api_1/runCode"
            "https://bogl.engr.oregonstate.edu/api_1/runCode"
            ,{
                method: "POST",
                headers: {
                  'Accept': 'application/json',
                  'Content-Type': 'application/json',
                  'Allow': '*'
                },
                body: JSON.stringify({
                    file    : code,
                    prelude : "",
                    input   : cmd,
                    buffer  : commandInput
                })
        }).then(function(res) {
          respStatus = res.status;
          return res.json();

        }).then(function(resp) {
          updateResults(results[x], cmd + "<br/><br/>" + parse_response(resp));

        }).catch((error) => {
          if((error instanceof SyntaxError || (error.name && error.name === "SyntaxError")) && respStatus === 504) {
            // gateway timeout
            //console.dir(error);
            print(" 🤖 BoGL Says: Unable to finish running your program, or not currently online. Double check your code, or check back later! ");

          } else if((error instanceof SyntaxError || (error.name && error.name === "SyntaxError"))) {
            // bad parse error
            //console.dir(error);
            print(" 🤖 BoGL Says: Your program was unable to be understood. Please double check it and try again! ");

          } else if((error instanceof TypeError || (error.name && error.name === "TypeError")) && respStatus === 0) {
            // likely JS disabled
            //console.dir(error);
            print(" 🤖 BoGL Says: Unable to execute your program. Make sure that Javascript is enabled and try again! ");

          } else if((error instanceof TypeError || (error.name && error.name === "TypeError"))) {
            // something else?
            //console.dir(error);
            print(" 🤖 BoGL Says: Unable to execute your program, please double check your code and try again. ");

          } else {
            // general error
            //console.dir(error);
            print(" 🤖 BoGL Says: An error occurred: " + error + " ");

          }
        });

      }));
    }
  });

})();
