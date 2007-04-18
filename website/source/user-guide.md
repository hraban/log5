{set-property html yes}
{set-property title Log5 User's Guide}
{set-property style-sheet user-guide}

# Log5 User's Guide

{table-of-contents :start-at 2 :depth 3}

## Introduction

So you'd like to add logging to your application and you've decided to use log5. Congratulations (and thank you!). Adding logging to an application is easy; the hard part is adding logging that helps you understand and track down problems.  This requires breaking your application into functional or logical units (what log5 calls categories) and adding logging messages in terms of these categories in all the right places. This overview first looks at log messages and then at how these messages are categorized. Finally, we'll see how to control what happens when log-messages run and how to configure this dynamically in your application.

## Overview

### Messages

Log messages should describe what an application is doing so that you can understand what went wrong or ensure that the right things happened in the right order. 
In effect, you're adding print statements that you can turn on and off whenever you want. (So whenever you're tempted to add some print or format statements during a debugging session, add log messages instead). A log message is just a call to `log-for`

    (log-for (application warn) "Widget ~s is misaligned by ~d degrees"
        widget out-of-whack)

A log-for macro expands into an array lookup which means it's very fast: don't be concerned about having a lot of them in your application. The second argument to log-for is a list of categories for the message; the third argument is a regular Lisp format control string and the rest of the arguments become arguments to format if the log message is actually triggered.


### Categories

A category is more like a tag in [del.icio.us][] than like a class. Categories can be simple like

    (defcategory file-system)

or complex like

    (defcategory operating-system 
     (and (or file-system memory process
          (not application))))

Categories are just a way to organize log messages; you can make them hierarchical if that works for you or you can make them more flexible. Note that log5 comes with the standard set of category levels:

    (defcategory fatal) 
    (defcategory error)
    (defcategory error+ (or error fatal))
    ...

A good general strategy is to build categories that fit the application and then create logging messages using an application category and a level category. 


### Senders

To collect log data, you must start a `sender`. Senders filter log messages based on a category specification and send the messages that pass on to their destination. For example, this stream-sender will write out (to `*error-output*` every log message that includes the tag `warn`, `error`, or `fatal` (because that is what `warn+` expands into).

    (start-sender 'warnings-and-worse
      (stream-sender :location *error-output*)
      :category-spec (warning+)
      :output-spec (time message load-average context))

The only unexplained bit is the `output-spec`. It describes the `outputs` of the sender.

### Outputs

Outputs are what a sender writes to its destination. They connect a name to a bit of code. For example, one of log5's predefined outputs is:

    (defoutput time (get-universal-time))

Whenever a sender's output-spec includes `time`, the log messages it sends will call `(get-universal-time)` and include it in the message. In addition to any outputs you define, log5 comes with several built-in ones including:


* time - gets the current universal time
* message - the formatted string from the log-for statement
* category - the category specification from the log-for statement
* context - the value of the current context (see `pop-context`, `push-context`, and `with-context ` below)

You should also note that your outputs can perform as much computation as you need them to. For example, this status-report output

    (defoutput status-report        (let ((path (find-unused-pathname              (merge-pathnames              (make-pathname                :name "status-report-"               :directory '(:relative "logs"))              (data-directory))             ".log")))          (with-new-file (*standard-output* path)        (status-report))          path))

prints a status-report to a log file every time it is part of a sender's output-spec. It then includes only the path to the report in the actual log file. Since you don't want logging to incur too much overhead, you should be judicious in the computations you place in outputs.


### Context

As alluded to in the description of outputs, log5 provides a place to put a description of the context in which logging occurs. This might be a file name in an analytical application or a session ID in a web app. The functions `push-context` and `pop-context` let you alter the context as needed and the macro `with-context` combined a push and a pop in the usual way.


## Miscellaneous Matters

### Configuring log5

One way to configure log5 is by placing calls to `start-sender` in your code. This might be a good thing to do for an error log that you're sure you'd always like to have running but it lacks flexibility! The better way is to choose a good place to put a configuration file and then use the `configure-from-file` method. A configuration file looks almost like a set of calls to start-sender (no XML madness for us!) using the format `(name (type [keyword value]*) (output*) (category*))`. For example:


    (index-log (log5:stream-sender 
               :location "/tmp/index.log")
               (time db-name log5:category log5:message 
                     os-process-id human-time)
               (and trace+ (or index merge)))


will start up a log to /tmp/index.log containing a log-messages about indexing and merging that are of level trace or higher. The configuration file is read
in the context of the current package with `*read-eval*` turned *off* and it is *not* evaluated (which I think avoids all security issues; if it doesn't, [shout out][mailing list] immediately!). You can, however, use the symbol `*standard-output*` to refer to `*standard-output*` (other special cases will be added as required).

### Implementation notes

Log5 aims to be easy, flexible *and* fast. What follows discusses some of the ways it aims for speed without sacrificing flexibility. Firstly, all category specifications are reduced to a (semi-)standard form and numbered. This includes both the categories you define with `defcategory` and categories that appear *on the fly* in calls to `log-for` (though these cannot define *new* categories). The point of this is that we can convert the category specification in a call to `log-for` into a number at compile time. {footnote Which means that changing categories upstream will alter the numbering so you need to be extra careful about dependencies and/or the occasional full recompile or your logging statements will get of sync; this is why it can be helpful to always include the `category` output in your sender's output-specs.} As we'll see in a minute, this means that we can check if a log-for call is active via a single array lookup.

When you create a sender, log5 builds an bit-vector whose size matches the total number of existing category specifications. It then checks if a category specification satisfies the sender's category specification by using `progv` to dynamically treat the categories as boolean variables and the sender specification as a boolean predicate. When we're done, the bit-vector will have a 1 in it for every category specification that satisfies the sender and a zero otherwise. This is how we can immediately check whether or not a `log-for` statement should actually trigger a log event.

Another source of speed is that senders do not call any generic functions at run-time. Instead, log5 builds and compiles an output function when the sender is created. Thus we only need to parse and interpret the output specification when the sender is created instead of once for every log message.

## Getting started

Getting started means defining some categories (with `defcategory`) and then adding log statements (with `log-for`). You can play around with logging using a `stream-sender` whose `location` is `*standard-output*`. Soon, you should start having ideas of what additional outputs you would like to see (and you can define those with `defoutput`) and where it might be helpful to have a `context`. Once things start to stabilize, you can write a configuration file and make sure that your application calls `configure-from-file` when it starts. 

If you questions, comments, or complaints, please contact the [mailing list][]. I'm also very open to collaboration (additional Senders, outputs, other ideas, etc).

## To do

The largest open issues for log5 are

* lack of self tests, 
* there is only a single sender (which works for files and streams), and
* log statements cannot be removed at compile time {footnote This is a nice feature that the Arnesi logger has and which will be stolen as soon as someone has time!}

There are probably many other missing bits and pieces; please let the [mailing list][] know.

<hr>

{footnotes}


 [del.icio.us]: http://del.icio.us
 [mailing list]: mailto:log5-devel@common-lisp.net
 [Arnesi]: http://common-lisp.net/project/bese/arnesi.html
