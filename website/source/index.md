{include header.md}
{set-property title Log5 - it's one more}

### Introduction 

Log5 is a Common Lisp logging framework organized around five things: categories, outputs, senders, messages and contexts. 

* *Categories* organize log messages into groups. They are like log4j's [Loggers][logger] only they're not necessarily arranged hierarchically; 

* *Senders* take care of getting log messages somewhere useful. The correspond to log4j's [Appenders][appender]; 

* the *Outputs* of a Sender specify what actually gets sent, printed, spoken, and so on.

* *Messages* are the actual log statements in your code

* and the *Context* provides a way to annotate log messages dynamically (useful, for example, if an application serves multiple clients). Log4j calls this a *[Nested Diagnostic Context][ndc]* but that's too high-faluting for me.

For more details, see the [overview][] or the [user-guide][].


### Getting Log5

A decent version of log5 has been implemented but be warned that the API is still in flux. 

You can help calm the flux by looking at the [darcs][] repository

    darcs get http://common-lisp.net/project/log5/darcs/log5

or by [ASDF-Installing][asdf-install] it or just downloading a [tarball][]. It's also on the [CLiki][log5-cliki]. Common-Lisp.net hosts the project's mailing list:

> [log5-devel@common-lisp.net][log5-mailing-list]

You can also follow development on [unCLog][].

Share peace!


{include footer.md}