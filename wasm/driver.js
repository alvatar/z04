var Module = typeof Module !== 'undefined' ? Module : {};

function TermEmu(element) {

    this.forced_input = "";
    this.forced_input_pos = 0;
    this.input = "";
    this.input_pos = 0;
    this.output_state = 0;
    this.output_pn = 0;
    this.output_params = Array(10);

    var self = this;

    if (ENVIRONMENT_IS_WEB) {
        self.textarea = element;
        element.addEventListener('keyup', function(event) {
            event.preventDefault();
        });
        element.addEventListener('keydown', function(event) {
            if (event.metaKey) {
                return; // on macOS process command key normally
            } else if (event.key === 'Enter') {
                self.add_input('\n');
            } else if (event.key === 'Tab') {
                self.add_input('\t');
            } else if (event.key === 'Escape') {
                self.add_input('\033');
            } else if (event.key === 'ArrowUp') {
                self.add_input('\033OA');
            } else if (event.key === 'ArrowDown') {
                self.add_input('\033OB');
            } else if (event.key === 'ArrowRight') {
                self.add_input('\033OC');
            } else if (event.key === 'ArrowLeft') {
                self.add_input('\033OD');
            } else if (event.key === 'Shift' ||
                       event.key === 'CapsLock' ||
                       event.key === 'Meta' ||
                       event.key === 'Alt' ||
                       event.key === 'Control') {
            } else if (event.key === 'Backspace') {
                if (element.selectionStart !== element.selectionEnd) {
                    element.setRangeText('', element.selectionStart, element.selectionEnd, 'end');
                } else {
                    self.add_input('\b');
                }
            } else if (event.key.length === 1) {
                if (event.ctrlKey && !(event.metaKey || event.altKey)) {
                    var c = event.key.charCodeAt(0);
                    if (c >= 97 && c <= 122) c -= 32; // 'a'..'z'
                    if (c >= 65 && c <= 90) {
                        c = c-64;
                        if (c === 3) { // ctrl-c
                            _user_interrupt();
                        } else {
                            self.add_input(String.fromCharCode(c));
                        }
                    }
                } else {
                    self.add_input(event.key);
                }
            } else {
                alert("key="+event.key + " code="+event.code);
            }
            event.preventDefault();
        });
        element.addEventListener('paste', function(event) {
            var text = event.clipboardData.getData('text');
            self.add_input(text);
            event.preventDefault();
        });
    } else if (ENVIRONMENT_IS_NODE) {
        process.stdin.on('readable', function() {
            var chunk;
            while ((chunk = process.stdin.read()) !== null) {
                self.add_input(chunk);
            }
        });
    }
}

TermEmu.prototype.add_input = function(str) {
    this.input += str;
};

TermEmu.prototype.set_forced_input = function(forced_input) {
    this.forced_input = forced_input;
    this.forced_input_pos = 0;
};

TermEmu.prototype.get_char = function() {
    if (this.forced_input_pos < this.forced_input.length) {
        return this.forced_input.charCodeAt(this.forced_input_pos++);
    }
    if (this.input_pos < this.input.length) {
        return this.input.charCodeAt(this.input_pos++);
    } else {
        this.input = "";
        this.input_pos = 0;
    }
    return undefined;
};

TermEmu.prototype.put_char = function(c) {
    if (ENVIRONMENT_IS_NODE) {
        this.add_output(c);
    } else if (this.output_state === 0) {
        if (c === 27) { // ESC
            this.output_state = 1;
        } else if (c === 8) { // backspace
            this.move_cursor_relative(-1);
        } else {
            this.add_output(c);
        }
    } else if (this.output_state === 1) { // after ESC
        if (c === 91) { // '['
            this.output_pn = 0;
            this.output_params[0] = 0;
            this.output_state = 2;
        } else {
            this.output_state = 0;
        }
    } else if (this.output_state === 2) { // accumulating parameters
        if (c >= 48 && c <= 57) { // '0'..'9'
            if (this.output_params[this.output_pn] < 1000)
                this.output_params[this.output_pn] =
                this.output_params[this.output_pn]*10 + (c-48);
        } else if (c === 59) { // ';'
            if (this.output_pn < this.output_params.length-1) {
                this.output_params[++this.output_pn] = 0;
            }
        } else {
            if (c === 65) { // 'A'
                var pos = this.output_params[0];
                if (pos === 0) pos = 1;
                this.move_cursor_relative_line(-pos)
            } else if (c === 66) { // 'B'
                var pos = this.output_params[0];
                if (pos === 0) pos = 1;
                this.move_cursor_relative_line(pos)
            } else if (c === 67) { // 'C'
                var pos = this.output_params[0];
                if (pos === 0) pos = 1;
                this.move_cursor_relative(pos)
            } else if (c === 68) { // 'D'
                var pos = this.output_params[0];
                if (pos === 0) pos = 1;
                this.move_cursor_relative(-pos)
            } else if (c === 72) { // 'H'
                this.move_cursor_absolute(0);
            } else if (c === 74) { // 'J'
                this.clear_from_cursor();
            } else if (c === 109) { // 'm'
                // ignore color change
            } else {
                alert('unhandled escape c='+c+ ' param='+this.output_params[0]);
            }
            this.output_state = 0;
        }
    } else {
        this.output_state = 0;
    }
    if (ENVIRONMENT_IS_WEB) {
        this.textarea.blur();
        this.textarea.focus();
    }
};

TermEmu.prototype.move_cursor_relative_line = function(rel_pos) {
    var element = this.textarea;
    var lines = element.value.split('\n');
    var curr_line = element.value.substr(0, element.selectionEnd).split('\n').length;
    var new_line = Math.max(0, Math.min(lines.length, curr_line + rel_pos - 1));
//    alert(rel_pos+" "+curr_line+" "+new_line+" "+lines.slice(0,new_line).join('\n').length+1);
    this.move_cursor_absolute(lines.slice(0,new_line).join('\n').length+1);
};
   
TermEmu.prototype.move_cursor_relative = function(rel_pos) {
    var element = this.textarea;
    var abs_pos = element.selectionEnd + rel_pos;
    this.move_cursor_absolute(abs_pos);
};

TermEmu.prototype.move_cursor_absolute = function(abs_pos) {
    var element = this.textarea;
    element.setSelectionRange(abs_pos, abs_pos, 'end');
};

TermEmu.prototype.clear_from_cursor = function() {
    var element = this.textarea;
    element.setRangeText('', element.selectionEnd, element.value.length, 'end');
};

TermEmu.prototype.add_output = function(c) {
    if (ENVIRONMENT_IS_WEB) {
        var element = this.textarea;
        var start = element.selectionEnd;
        var end = start;
        if (element.value[start] !== '\n') end++;
        element.setRangeText(String.fromCharCode(c), start, end, 'end');
    } else if (ENVIRONMENT_IS_NODE) {
        process.stdout.write(String.fromCharCode(c));
    }
};

function localStorageGet(item) {
    if (ENVIRONMENT_IS_WEB) {
        var val = localStorage.getItem(item);
        if (typeof val !== 'string') val = '';
        return val;
    } else {
        return '';
    }
}

function localStorageSet(item, val) {
    if (ENVIRONMENT_IS_WEB) {
        localStorage.setItem(item, val);
    }
}

function run_scheme() {
    var wait = _idle();
    if (wait < 0) {
        _cleanup();
    } else {
        //console.log("wait=" + wait + "   now=" + Date.now()/1000);
        setTimeout(run_scheme, Math.max(0, Math.round(1000*wait)));
    }
};

Module['onRuntimeInitialized'] = function() {
    _setup();
    run_scheme();
};

Module['preInit'] = [function() {

    var element;

    if (ENVIRONMENT_IS_WEB) {
        element = document.getElementById('gambit-repl');
        if (!element) element = document.getElementById('output');
        if (!element) {
            element = document.createElement('style');
            element.innerText = " \
                #gambit-repl { \
                  width: 100%; \
                  height: 200px; \
                  margin: 0 auto; \
                  margin-top: 10px; \
                  border-left: 2px; \
                  border-right: 2px; \
                  padding-left: 2px; \
                  padding-right: 2px; \
                  display: block; \
                  background-color: black; \
                  color: white; \
                  font-family: 'Lucida Console', Monaco, monospace; \
                  outline: none; \
                } \
            ";
            document.body.appendChild(element);
            element = document.createElement('textarea');
            element.setAttribute('id', 'gambit-repl');
            document.body.appendChild(element);
        }
    }

    // setup the terminal emulator
    var termEmu = new TermEmu(element);

    // force initial input
    //termEmu.set_forced_input('(define (f x) (if (< x 2) x (+ (f (- x 1)) (f (- x 2)))))\n(f 10)\n');

    // Patch TTY.stream_ops.read to read from emulated terminal

    TTY.default_tty_ops.get_char = function(tty) {
        var c = termEmu.get_char();
        if (c !== undefined) return c;
        return undefined; // will generate a EAGAIN error
    };

    TTY.default_tty_ops.put_char = function(tty, val) {
        termEmu.put_char(val);
    };
}];
