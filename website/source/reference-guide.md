{include header.md}

{set-property html yes}
{set-property title "Log5 Reference Guide"}
{set-property style-sheet user-guide}
{set-property docs-package log5}

# Log5 Reference Guide

{table-of-contents :start 2 :depth 3}

<div class='reference'>

## The Log5 Manager

{docs ignore-errors-p}
{docs log-manager}
{docs output-specs}
{docs category-specs}
{docs id->category}

## Dealing with Context

{docs context}
{docs pop-context}
{docs push-context}
{docs with-context}
{docs with-context-for}

{docs log-message}
{docs log-if}
{docs log-for}
{docs when-logging}


## Senders 

### Starting, stopping and seeing 

{docs senders}
{docs start-sender}
{docs start-stream-sender}
{docs stop-sender}
{docs stop-all-senders}

## Using Log5 as a debugging tool

{docs debug-category-spec}
{docs undebug-category-spec}

## Categories

fatal error warn info trace dribble
error+ warn+ info+ trace+ dribble+

{docs defcategory}

## Output

time category message

{docs defoutput}

## Configuring Log5

### At compile-time

{docs compile-category-spec}

### At run-time (or load-time)

{docs configuration-file}
{docs configure-from-file))}


## Getting at the guts

{docs basic-sender}
{docs sender-with-categories}
{docs stream-sender-mixin}
{docs stream-sender}
{docs location}
{docs output-spec}
{docs category-spec}
{docs close-sender}
{docs close-stream?}
{docs output-stream}
{docs name}
{docs create-handle-message-context}
{docs start-handling}
{docs finish-handling}


</div>

## Indices

### Index of Functions

{docs-index function}

### Index of variables

{docs-index variable}

### Index of Macros

{docs-index macro}

### Full symbol index

{docs-index :all}

<hr>

{footnotes}

{include footer.md}