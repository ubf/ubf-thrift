

#Universal Binary Format and Thrift#


Copyright (c) 2011-2012 by Joseph Wayne Norton

__Authors:__ Joseph Wayne Norton ([`norton@alum.mit.edu`](mailto:norton@alum.mit.edu)).<p>This is UBF-THRIFT, a framework for integrating UBF, TBF, and Thrift.
This repository depends on the ubf open source repository.</p>
<p><em>This repository is intended for production deployment and is deployed
in carrier-grade systems.</em></p>

<h2 id="_quick_start_recipe">Quick Start Recipe</h2>

<p>To download, build, and test the ubf_thrift application in one shot,
please follow this recipe:</p>


<pre><tt>$ mkdir working-directory-name
$ cd working-directory-name
$ git clone https://github.com/ubf/ubf-thrift.git ubf_thrift
$ cd ubf_thrift
$ ./rebar get-deps
$ ./rebar clean
$ ./rebar compile
$ ./rebar eunit</tt></pre>

<p>For an alternative recipe with other "features" albeit more complex,
please read further.</p>



<h2 id="_documentation">Documentation</h2>


<h3 id="_where_should_i_start">Where should I start?</h3>
<p>This README is a good first step.</p>
<p>The UBF User's Guide is the best next step.  Check out
<a href="http://ubf.github.com/ubf/ubf-user-guide.en.md">http://ubf.github.com/ubf/ubf-user-guide.en.html</a> for further
detailed information.</p>
<p>Eunit tests can be found in the test/eunit directory.  These tests
illustrate a generic module that uses UBF's contract manager for
checking Thrift requests and responses.</p>


<h3 id="_what_is_ubf">What is UBF?</h3>
<p>UBF is the "Universal Binary Format", designed and implemented by Joe
Armstrong.  UBF is a language for transporting and describing complex
data structures across a network.  It has three components:</p>
<ul>
<li>
<p>
UBF(a) is a "language neutral" data transport format, roughly
  equivalent to well-formed XML.
</p>
</li>
<li>
<p>
UBF(b) is a programming language for describing types in UBF(a) and
  protocols between clients and servers.  This layer is typically
  called the "protocol contract".  UBF(b) is roughly equivalent to
  Verified XML, XML-schemas, SOAP and WDSL.
</p>
</li>
<li>
<p>
UBF(c) is a meta-level protocol used between a UBF client and a UBF
  server.
</p>
</li>
</ul>
<p>See <a href="http://norton.github.com/ubf">http://norton.github.com/ubf</a> for further details.</p>


<h3 id="_what_is_thrift">What is Thrift?</h3>
<p>Thrift is a remote procedure call protocol.  See
<a href="http://incubator.apache.org/thrift/">http://incubator.apache.org/thrift/</a> for full details.</p>




<h2 id="_to_download">To download</h2>

<ol class="arabic">
<li>
<p>
Configure your e-mail and name for Git
</p>


<pre><tt>$ git config \--global user.email "you@example.com"
$ git config \--global user.name "Your Name"</tt></pre>

</li>
<li>
<p>
Install Repo
</p>


<pre><tt>$ mkdir -p ~/bin
$ wget -O - https://dl-ssl.google.com/dl/googlesource/git-repo/repo > ~/bin/repo
$ chmod a+x ~/bin/repo</tt></pre>

</li>
<li>
<p>
Create working directory
</p>


<pre><tt>$ mkdir working-directory-name
$ cd working-directory-name
$ repo init -u https://github.com/ubf/manifests.git -m ubf-thrift-default.xml</tt></pre>


<table><tr>
<td class="icon">
Note
</td>
<td class="content">Your "Git" identity is needed during the init step.  Please
enter the name and email of your GitHub account if you have one.  Team
members having read-write access are recommended to use "repo init -u
<a href="mailto:git@github.com">git@github.com</a>:ubf/manifests.git -m ubf-thrift-default-rw.xml".</td>
</tr></table>


<table><tr>
<td class="icon">
Tip
</td>
<td class="content">If you want to checkout the latest development version, please
append " -b dev" to the repo init command.</td>
</tr></table>

</li>
<li>
<p>
Download Git repositories
</p>


<pre><tt>$ cd working-directory-name
$ repo sync</tt></pre>

</li>
</ol>
<p>For futher information and help for related tools, please refer to the
following links:</p>
<ul>
<li>
<p>
Erlang - <a href="http://www.erlang.org/">http://www.erlang.org/</a>
</p>
<ul>
<li>
<p>
<strong>R13B04 or newer, R15B has been tested most recently</strong>
</p>
</li>
</ul>
</li>
<li>
<p>
Git - <a href="http://git-scm.com/">http://git-scm.com/</a>
</p>
<ul>
<li>
<p>
<strong>Git 1.5.4 or newer, Git 1.7.9.3 has been tested recently</strong>
</p>
</li>
<li>
<p>
<em>required for Repo and GitHub</em>
</p>
</li>
</ul>
</li>
<li>
<p>
GitHub - <a href="https://github.com">https://github.com</a>
</p>
</li>
<li>
<p>
Python - <a href="http://www.python.org">http://www.python.org</a>
</p>
<ul>
<li>
<p>
<strong>Python 2.4 or newer, Python 2.7.1 has been tested most recently
    (CAUTION: Python 3.x might be too new)</strong>
</p>
</li>
<li>
<p>
<em>required for Repo</em>
</p>
</li>
</ul>
</li>
<li>
<p>
Rebar - <a href="https://github.com/basho/rebar/wiki">https://github.com/basho/rebar/wiki</a>
</p>
</li>
<li>
<p>
Repo - <a href="http://source.android.com/source/git-repo.md">http://source.android.com/source/git-repo.html</a>
</p>
</li>
</ul>



<h2 id="_to_build_basic_recipe">To build - basic recipe</h2>

<ol class="arabic">
<li>
<p>
Get and install an erlang system <a href="http://www.erlang.org">http://www.erlang.org</a>
</p>
</li>
<li>
<p>
Build
</p>


<pre><tt>$ cd working-directory-name
$ make compile</tt></pre>

</li>
<li>
<p>
Run the unit tests
</p>


<pre><tt>$ cd working-directory-name
$ make eunit</tt></pre>

</li>
</ol>



<h2 id="_to_build_optional_features">To build - optional features</h2>

<ol class="upperalpha">
<li>
<p>
Dialyzer Testing <em>basic recipe</em>
</p>
<ol class="arabic">
<li>
<p>
Build Dialyzer's PLT <em>(required once)</em>
</p>


<pre><tt>$ cd working-directory-name
$ make build-plt</tt></pre>


<table><tr>
<td class="icon">
Tip
</td>
<td class="content">Check Makefile and dialyzer's documentation for further
information.</td>
</tr></table>

</li>
<li>
<p>
Dialyze with specs
</p>


<pre><tt>$ cd working-directory-name
$ make dialyze</tt></pre>


<table><tr>
<td class="icon">
Caution
</td>
<td class="content">If you manually run dialyzer with the "-r" option, execute
"make clean compile" first to avoid finding duplicate beam files
underneath rebar's .eunit directory.  Check Makefile for further
information.</td>
</tr></table>

</li>
<li>
<p>
Dialyze without specs
</p>


<pre><tt>$ cd working-directory-name
$ make dialyze-nospec</tt></pre>

</li>
</ol>
</li>
</ol>



<h2 id="_credits">Credits</h2>

<p>Many, many thanks to Joe Armstrong, UBF's designer and original
implementor.</p>
<p>Gemini Mobile Technologies, Inc. has approved the release of this
repository under an MIT license.</p>




##Modules##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="https://github.com/ubf/ubf-thrift/blob/master/doc/ftbf.md" class="module">ftbf</a></td></tr>
<tr><td><a href="https://github.com/ubf/ubf-thrift/blob/master/doc/ftbf_driver.md" class="module">ftbf_driver</a></td></tr>
<tr><td><a href="https://github.com/ubf/ubf-thrift/blob/master/doc/tbf.md" class="module">tbf</a></td></tr>
<tr><td><a href="https://github.com/ubf/ubf-thrift/blob/master/doc/tbf_driver.md" class="module">tbf_driver</a></td></tr>
<tr><td><a href="https://github.com/ubf/ubf-thrift/blob/master/doc/thrift_contract_parser.md" class="module">thrift_contract_parser</a></td></tr>
<tr><td><a href="https://github.com/ubf/ubf-thrift/blob/master/doc/ubf_thrift_plugin.md" class="module">ubf_thrift_plugin</a></td></tr></table>

