{include resources/header.md}

<div class="contents">
<div class="system-links">

  * [Mailing Lists][4]
  * [Getting it][5]
  * [Documentation][6]
  * [News][7]
  * [Changelog][8]

   [4]: #mailing-lists
   [5]: #downloads
   [6]: documentation/ (documentation link)
   [7]: #news
   [8]: changelog.html

</div>
<div class="system-description">

### How it works

Tinaa is based on the idea that a system is made up of parts and subparts and sub-subparts (all resting on the [back of a turtle][9]). Tinaa can document anything as long as you tell it:

   [9]: http://en.wikipedia.org/wiki/Turtles_all_the_way_down (Wikipedia article on 'turtles all the way down')

  1. What kind of sub-parts it has
  2. How to iterate over the sub-parts
  3. How to display information about the parts

{anchor mailing-lists}

### Mailing Lists

  * [tinaa-devel][11]: A list for questions, patches, bug reports, and so on; It's for everything other than announcements.

   [11]: http://common-lisp.net/cgi-bin/mailman/listinfo/tinaa-devel

{anchor downloads}

### Getting it

A [Darcs][12] repository is available. The commands are listed below:
    
[12]: http://www.darcs.net/

    darcs get http://common-lisp.net/project/tinaa/

Tinaa is [ASDF installable][13]. Its CLiki home is right [where][14] you'd expect.

   [13]: http://www.cliki.net/asdf-install
   [14]: http://www.cliki.net/tinaa

There's also a handy [gzipped tar file][15].

   [15]: http://common-lisp.net/project/tinaa/tinaa_latest.tar.gz

### What it does now

Tinaa currently knows how to document packages (consisting of variables, constants, functions, classes, generic function, macros and symbols) and ASDF-Systems (consisting of sub-systems and packages). [Here][16] is Tinaa's self-documentation.

   [16]: documentation/index.html

### What it doesn't do

There are lots things it could do more nicely (class diagrams, prettier tables, callers and callees / cross referencing and so forth). 

### What is happening

<table class="system-news">
<tr>
	<th>
		1 March 2008
	</th>
	<td>
		Tagged version 0.5.6 to clean up a bit of bit-rot. It's been a busy year for me and a slow year for Tinaa!
</tr>
<tr>
	<th>
		1 January 2007
	</th>
	<td>
		Tagged version 0.5.3; Adds ASDF-System-Connection between Tinaa and CL-Markdown. If CL-Markdown is loaded then all docstrings are run through Markdown.</td>
</tr>
<tr>
	<th>
		23 September 2006
	</th>
	<td>
		Tagged version 0.5.1; minor cleanup, added *css-file* special variable, can call document-system with non-Tinaa package symbols for the system-kind.
	</td>
</tr>
<tr>
	<th>
		2 Feb 2006
	</th>
	<td>
		Improved style sheet; many minor bug fixes. Investigating SBCL support (see this [paste][17] for details...)
	</td>
</tr>
<tr>
	<th>
		17 Dec 2005
	</th>
	<td>
		Modulo some minor details, Tinaa now works with Allegro 7.0
	</td>
</tr>
</table>
		
</div>
</div>

{include resources/footer.md}
