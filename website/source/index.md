{include header.md}
{set-property title "Log5 - it's one more"}

### Introduction 

Log5 is a Common Lisp logging framework organized around five things: categories, outputs, senders, messages and contexts. 

* *Categories* organize log messages into groups. They are like log4j's [Loggers][logger] only they're not necessarily arranged hierarchically; 

* *Senders* take care of getting log messages somewhere useful. The correspond to log4j's [Appenders][appender]; 

* the *Outputs* of a Sender specify what actually gets sent, printed, spoken, and so on.

* *Messages* are the actual log statements in your code

* and the *Context* provides a way to annotate log messages dynamically (useful, for example, if an application serves multiple clients). Log4j calls this a *[Nested Diagnostic Context][ndc]* but that's too high-faluting for me.

For more details, see the [overview][] or the [user-guide][].


### Getting Log5

log5 is implemented and stabile.  
                                  
metabang.com is slowly switch from [darcs][] to [git][] for source control; the current log5 repository is on [github][github-log5] and you can clone it using:

    git clone git://github.com/gwkkwg/log5

You can use [ASDF-Installing][asdf-install] or just download a [tarball][]. It's also on the [CLiki][log5-cliki]. Common-Lisp.net hosts the project's mailing list:

> [log5-devel@common-lisp.net][log5-mailing-list]

You can also follow development (such as it is :-)) on [unCLog][].

Share peace!


### Latest News 
   
15 February 2009 - how time flies. moved to git

22 December 2007 - Some bug fixes and minor tweaks

29 June 2007 - log5 is updated with compile-time log categories and a handy debugging facility. 

19 April 2007 - log5 has seen a bunch of minor development over the last few months. The biggest changes are

* The :output-specs and :category-specs arguments to `start-sender` are now evaluated. So instead of

        (start-sender ... :output-specs (time message))
    
    you must write

        (start-sender ... :output-specs '(time message))
    
    (Note the added quote).

* I fixed a horrible performance bug which caused all the arguments to `log-for` to be evaluated regardless of whether or not the logger was active (ack!)

* I fixed another performance bug in the file-stream handling. This bug also caused a problem when logging to string streams under Allegro.


{include footer.md}
