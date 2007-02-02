{include header.md}
{set-property title Log5 - it's one more}

### Introduction 

Log5 is a Common Lisp logging framework organized around five things: categories, outputs, senders, messages and contexts. 


* *Categories* organize log messages into groups. They are like log4j's [Loggers][logger] only they're not necessarily arranged hierarchically; 

* *Senders* take care of getting log messages somewhere useful. The correspond to log4j's [Appenders][appender]; 

* the *Outputs* of a Sender specify what actually gets sent, printed, spoken, and so on.

* *Messages* are the actual log statements in your code

* and the *Context* provides a way to annotate log messages dynamically (useful, for example, if an application serves multiple clients). Log4j calls this a *[Nested Diagnostic Context][ndc]* but that's too high-faluting for me.

For more details, you can keep reading or see the [user-guide][].

### Overview 

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



### Getting Log5

I've implemented most of a prototype of log5 and am currently experimenting with syntax and efficiency. 

You can help me experiment by looking at the [darcs][] repository

    darcs get http://common-lisp.net/project/log5/darcs/log5

or by [ASDF-Installing][asdf-install] it or just downloading a [tarball][]. It's also on the [CLiki][log5-cliki]. Common-Lisp.net hosts the project's mailing list:

> [log5-devel@common-lisp.net][log5-mailing-list]

You can also follow development on [unCLog][].

Share peace!

 [log5-mailing-list]: mailto:log5-devel@common-lisp.net

 [unCLog]: http://unclog.metabang.com/

 [ndc]: http://logging.apache.org/log4j/docs/api/org/apache/log4j/NDC.html

 [logger]: http://logging.apache.org/log4j/docs/api/org/apache/log4j/Logger.html

 [appender]: http://logging.apache.org/log4j/docs/api/org/apache/log4j/Appender.html

{include footer.md}