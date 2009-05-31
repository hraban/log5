{include header.md}
{set-property title "Log5 - overview"}

## Overview 

{table-of-contents :start 2 :depth 3}

### Categories, senders and messages, oh my!

the bird's eye view looks like this: You define *categories* for your application. These might look like

    (defcategory motion)

    (defcategory energy)

    (defcategory physics (or energy motion))

    (defcategory planner)

and so forth. Categories are sort of like Lisp `*features*` with names. They can be simple (like `motion`) or boolean combinations (like `physics`). When you write a typical log *message*, you use a combination of categories to describe it:

    (log-for (and physics (not file-system) trace)
    	     "starting widget simulation")
    
or 

    (log-for (or planner motion) "Planning path for agent ~a" (name agent))

You start a *sender* using `start-sender` (surprise!). You specify what kind of sender it is (e.g., a stream sender or a database sender or an HTML sender or whatever) and pass along whatever arguments are needed to create it. You also specify the categories and the *outputs* the sender will send. Categories were discussed above; a sender's outputs are a list of named properties defined with defoutput. For example: 

    (defoutput time (get-universal-time))

    (defoutput virtual-memory (os-get-virtual-memory))

    (defoutput current-database (name *db*)))

Outputs can compute anything that makes sense for your program (though they ought to compute it quickly if you don't want logging to kill performance!). Some outputs are special and predefined. For example, the output `message` refers to the string created by the log message statement (e.g., the `log-for` examples above). The output `context` refers to the current *context* (the last of our five players).

The context is a carry-all you can use to specify whatever important is happening in the global environment. If you're writing a web-application; the context might track the current session ID; A planner might track the current agent and so forth. Information from the context is added to the end of each log message sent and so functions as a variable portion in contrast to the fixed structure of the sender's output.

### Debugging with log5

You can also use log5 in debugging. The [log-manager][] includes a debug console to which log messages can be sent. Use [debugging][] and [undebugging][] to tell log5 which categories you want to see. Let's use this code for to explain:

    (defun run-program ()
      (log-for info "enter run")
      (step-1)
      (log-for info "exit run"))

    (defun step-1 ()
      (log-for trace "enter step-1")
      (sub-step-1)
      (log-for trace "exit step-1"))

    (defun sub-step-1 ()
      (log-for dribble "enter sub-step-1")
      (log-for dribble "exit sub-step-1"))

If I just evaluate `(run-program)`, then I'll see no output. Suppose that I decide to debug at the lowest level of detail: `dribble`:

    > (debugging 'dribble+)
    (or dribble+)
    
    > (run-program)
    "enter run"
    "enter step-1"
    "enter sub-step-1"
    "exit sub-step-1"
    "exit step-1"
    "exit run"

If I just want to see the high-level structure, I could debug at the `info` level. I can also change the `output-spec` used by the console. For example, I might want to see the time when each event occurs:

    > (debugging 'info+ :reset? t :output-spec 'time)
    (or info+)
    
    > (run-program)
    3452689957 "enter run"
    3452689957 "exit run"



{include footer.md}