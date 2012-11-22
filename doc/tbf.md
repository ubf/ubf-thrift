

#Module tbf#
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


<p>Functions for Thrift(Binary)<[8594,69,114,108,97,110,103,32,100,97,116,97,32,99,111,110,118,101,114,115,
 105,111,110,46]</p>


<pre><code>For most purposes, these functions are not called by code outside
of this library: Erlang client &amp; Erlang server application code
usually have no need to use these functions.</code></pre>



<pre><code>== Links</code></pre>



<pre><code><ul>
<li> http://incubator.apache.org/thrift </li>
</ul></code></pre>



<pre><code>== Thrift Basic Types (ABNF)
------
message        =  message-begin struct message-end
message-begin  =  method-name message-type message-seqid
message-end    =  ""
method-name    =  STRING
message-type   =  T-CALL/ T-REPLY/ T-EXCEPTION/ T-ONEWAY
message-seqid  =  I32</code></pre>



<pre><code>struct         =  struct-begin *field field-stop struct-end
struct-begin   =  struct-name
struct-end     =  ""
struct-name    =  STRING ;; NOTE: struct-name is not written to nor read from the network
field-stop     =  T-STOP</code></pre>



<pre><code>field          =  field-begin field-data field-end
field-begin    =  field-name field-type field-id
field-end      =  ""
field-name     =  STRING ;; NOTE: field-name is not written to nor read from the network
field-type     =  T-STOP/ T-VOID/ T-BOOL/ T-BYTE/ T-I08/ T-I16/ T-I32/ T-U64/ T-I64/ T-DOUBLE/
                  T-BINARY/ T-STRUCT/ T-MAP/ T-SET/ T-LIST
field-id       =  I16
field-data     =  BOOL/ I08/ I16/ I32/ U64/ I64/ DOUBLE/ BINARY/
                  struct/ map/ list/ set
field-datum    =  field-data field-data</code></pre>



<pre><code>map            =  map-begin *field-datum map-end
map-begin      =  map-key-type map-value-type map-size
map-end        =  ""
map-key-type   =  field-type
map-value-type =  field-type
map-size       =  I32</code></pre>



<pre><code>list           =  list-begin *field-data list-end
list-begin     =  list-elem-type list-size
list-end       =  ""
list-elem-type =  field-type
list-size      =  I32</code></pre>



<pre><code>set            =  set-begin *field-data set-end
set-begin      =  set-elem-type set-size
set-end        =  ""</code></pre>



<pre><code>set-elem-type  =  field-type
set-size       =  I32</code></pre>



<pre><code>------</code></pre>



<pre><code>== Thrift (Binary) Core Types (ABNF)
------
BOOL           =  %x00/ %x01         ; 8/integer-signed-big
BYTE           =  OCTET              ; 8/integer-signed-big
I08            =  OCTET              ; 8/integer-signed-big
I16            =  2*OCTET            ; 16/integer-signed-big
I32            =  4*OCTET            ; 32/integer-signed-big
U64            =  8*OCTET            ; 64/integer-unsigned-big
I64            =  8*OCTET            ; 64/integer-signed-big
DOUBLE         =  8*OCTET            ; 64/float-signed-big
STRING         =  I32 UTF8-octets
BINARY         =  I32 *OCTET</code></pre>



<pre><code>T-CALL         =  %x01
T-REPLY        =  %x02
T-EXCEPTION    =  %x03
T-ONEWAY       =  %x04</code></pre>



<pre><code>T-STOP         =  %x00
T-VOID         =  %x01
T-BOOL         =  %x02
T-BYTE         =  %x03
T-I08          =  %x05
T-I16          =  %x06
T-I32          =  %x08
T-U64          =  %x09
T-I64          =  %x0a
T-DOUBLE       =  %x04
T-BINARY       =  %x0b
T-STRUCT       =  %x0c
T-MAP          =  %x0d
T-SET          =  %x0e
T-LIST         =  %x0f</code></pre>



<pre><code>------</code></pre>



<pre><code>== Mapping: Thrift Types (Erlang)
------
tbf::message() = {'message', tbf::method_name(), tbf::message_type(), tbf::message_seqid(), tbf::struct()}.
tbf::method_name() = binary().
tbf::message_type() = 'T-CALL' | 'T-REPLY' | 'T-EXCEPTION' | 'T-ONEWAY'.
tbf::message_seqid() = integer().</code></pre>



<pre><code>tbf::struct() = {'struct', tbf::struct_name(), [tbf::field()]}.
tbf::struct_name() = binary().</code></pre>



<pre><code>tbf::field() = {'field', tbf::field_name(), tbf::field_type(), tbf::field_id(), tbf::field_data()}.
tbf::field_name() = binary().
tbf::field_type() = 'T-STOP' | 'T-VOID' | 'T-BOOL' | 'T-BYTE'
                  | 'T-I08' | 'T-I16' | 'T-I32' | 'T-U64' | 'T-I64' | 'T-DOUBLE'
                  | 'T-BINARY' | 'T-STRUCT' | 'T-MAP' | 'T-SET' | 'T-LIST'.
tbf::field_id() = integer().
tbf::field_data() = tbf::void() | tbf::boolean() | integer()
                  | integer() | float()
                  | binary() | tbf::struct() | tbf::map() | tbf::set() | tbf::list().</code></pre>



<pre><code>tbf::map() = {'map', tbf::map_type(), [tbf::map_data()]}.
tbf::map_type() = {tbf::field_type(), tbf::field_type()}.
tbf::map_data() = {tbf::field_data(), tbf::field_data()}.</code></pre>



<pre><code>tbf::set() = {'set', tbf::set_type(), [tbf::set_data()]}.
tbf::set_type() = tbf::field_type().
tbf::set_data() = tbf::field_data().</code></pre>



<pre><code>tbf::list() = {'list', tbf::list_type(), [tbf::list_data()]}.
tbf::list_type() = tbf::field_type().
tbf::list_data() = tbf::field_data().</code></pre>



<pre><code>tbf::void() = 'undefined'.
tbf::boolean() = 'true' | 'false'.</code></pre>



<pre><code>------</code></pre>



<pre><code>== Mapping: UBF Types (Erlang)
------
ubf::tuple() = tuple().</code></pre>



<pre><code>ubf::list() = list().</code></pre>



<pre><code>ubf::number = integer() | float().</code></pre>



<pre><code>ubf::string() = {'$S', [integer()]}.</code></pre>



<pre><code>ubf::proplist() = {'$P', [{term(), term()}]}.</code></pre>



<pre><code>ubf::binary() = binary().</code></pre>



<pre><code>ubf::boolean() = 'true' | 'false'.</code></pre>



<pre><code>ubf::atom() = atom().</code></pre>



<pre><code>ubf::record() = record().</code></pre>



<pre><code>ubf::term() = ubf::tuple() | ubf::list() | ubf::number()
            | ubf::string() | ubf::proplist() | ubf::binary()
            | ubf::boolean() | ubf::atom() | ubf::record().</code></pre>



<pre><code>ubf::state() = ubf::atom().</code></pre>



<pre><code>ubf::request() = ubf::term().
ubf::response() = {ubf::term(), ubf::state()}. % {Reply,NextState}</code></pre>



<pre><code>ubf:event_in() = {event_in, ubf::term()}.
ubf:event_out() = {event_out, ubf::term()}.</code></pre>



<pre><code>------</code></pre>



<pre><code>== UBF Messages
------
Remote Procedure Call (Client -> Server -> Client)
  ubf::request() => ubf::response().</code></pre>



<pre><code>Asynchronous Event (Server -> Client)
  'EVENT' => ubf::event_out().</code></pre>



<pre><code>Asynchronous Event (Server <- Client)
  'EVENT' <= ubf::event_in().</code></pre>



<pre><code>------</code></pre>



<pre><code>== Mapping: Thrift Messages&lt;->UBF Messages
------
Remote Procedure Call (Client -> Server -> Client)
 ubf::request() = tbf::message().
 ubf::response() = tbf::message().</code></pre>



<pre><code>Asynchronous Event (Server -> Client)
  ubf:event_out() = tbf::message().</code></pre>



<pre><code>Asynchronous Event (Server <- Client)
  ubf:event_in() = tbf::message().</code></pre>



<pre><code>------</code></pre>



<pre><code>NOTE: Thrift has no concept of a UBF 'state' so it is not returned
to the thrift client as a part of the rpc response.  This is
enabled by the 'simplerpc' option.</code></pre>



<pre><code>TBD: Is there a Thrift-specific way to handle the following error cases?
<ul>
<li> encoding/decoding errors </li>
<li> server breaks contract </li>
<li> client breaks contract </li>
</ul></code></pre>



<pre><code>== Mapping: Thrift Types&lt;-> UBF 'Native' Types
------</code></pre>



<pre><code>ubf::tuple() = {'struct', <<"$T">>, [{'field', <<>>, 'T-LIST', 1, {'list', 'T-STRUCT', [ubf::term()]}}]{1} }.</code></pre>



<pre><code>ubf::list() = {'struct', <<"$L">>, [{'field', <<>>, 'T-LIST', 1, {'list', 'T-STRUCT', [ubf::term()]}}]{1} }.</code></pre>



<pre><code>ubf::number = {'struct', <<"$N">>, [{'field', <<>>, 'T-I64', 1, integer()}]{1} | [{'field', <<>>, 'T-DOUBLE', 1, float()}]{1} }.</code></pre>



<pre><code>ubf::string() = {'struct', <<"$S">>, [{'field', <<>>, 'T-BINARY', 1, binary()}]{1} }.</code></pre>



<pre><code>ubf::proplist() = {'struct', <<"$P">>, [{'field', <<>>, 'T-MAP', 1, {'map', 'T-STRUCT', 'T-STRUCT', [{ubf::term(),ubf::term()}]}}]{1} }.</code></pre>



<pre><code>ubf::binary() = {'struct', <<"$B">>, [{'field', <<>>, 'T-BINARY', 1, binary()}]{1} }.</code></pre>



<pre><code>ubf::boolean() = {'struct', <<"$O">>, [{'field', <<>>, 'T-BOOL', 1, boolean()}]{1} }.</code></pre>



<pre><code>ubf::atom() = {'struct', <<"$A">>, [{'field', <<>>, 'T-BINARY', 1, binary()}]{1} }.</code></pre>



<pre><code>ubf::record() = {'struct', <<"$R">>, [{'field', <<>>, 'T-MAP', 1, {'map', 'T-BINARY', 'T-STRUCT', [{binary(),ubf::term()}]}}]{1} }.
  NOTE: A record's name is stored by a special key {<<>>, ubf::atom()} in the map.</code></pre>



<pre><code>ubf::term() = ubf::tuple() | ubf::list() | ubf::number()
            | ubf::string() | ubf::proplist() | ubf::binary()
            | ubf::boolean() | ubf::atom() | ubf::record().</code></pre>



<pre><code>ubf::state() = ubf::atom().</code></pre>



<pre><code>ubf::request() = ubf::term().
ubf:response() = {ubf::term(), ubf::state()}. % {Reply,NextState}</code></pre>



<pre><code>ubf:event_in() = {event_in, ubf::term()}.
ubf:event_out() = {event_out, ubf::term()}.</code></pre>



<pre><code>------</code></pre>



<pre><code>== Mapping: Thrift Messages&lt;->UBF 'Native' Messages
------
Remote Procedure Call (Client -> Server -> Client)
 ubf::request() = {'message', <<"$UBF">>, 'T-CALL', tbf::message_seqid(), ubf::term()}.
 ubf::response() = {'message', <<"$UBF">>, 'T-REPLY', tbf::message_seqid(), ubf::term()}.</code></pre>



<pre><code>Asynchronous Event (Server -> Client)
  ubf:event_out() = {'message', <<"$UBF">>, 'T-ONEWAY', tbf::message_seqid(), ubf::term()}.</code></pre>



<pre><code>Asynchronous Event (Server <- Client)
  ubf:event_in() = {'message', <<"$UBF">>, 'T-ONEWAY', tbf::message_seqid(), ubf::term()}.</code></pre>



<pre><code>------</code></pre>
.

__Behaviours:__ [`contract_proto`](contract_proto.md).
<a name="types"></a>

##Data Types##




###<a name="type-cont">cont()</a>##



<pre>cont() = {more, function()}</pre>



###<a name="type-error">error()</a>##



<pre>error() = {error, Reason::term()}</pre>



###<a name="type-ok">ok()</a>##



<pre>ok() = {done, Output::term(), Remainder::binary(), VSN::integer()}</pre>
<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#atom_to_binary-1">atom_to_binary/1</a></td><td></td></tr><tr><td valign="top"><a href="#binary_to_atom-1">binary_to_atom/1</a></td><td></td></tr><tr><td valign="top"><a href="#binary_to_existing_atom-1">binary_to_existing_atom/1</a></td><td></td></tr><tr><td valign="top"><a href="#contract_records-0">contract_records/0</a></td><td></td></tr><tr><td valign="top"><a href="#decode-1">decode/1</a></td><td></td></tr><tr><td valign="top"><a href="#decode-2">decode/2</a></td><td></td></tr><tr><td valign="top"><a href="#decode-3">decode/3</a></td><td></td></tr><tr><td valign="top"><a href="#decode_init-0">decode_init/0</a></td><td></td></tr><tr><td valign="top"><a href="#decode_init-1">decode_init/1</a></td><td></td></tr><tr><td valign="top"><a href="#decode_init-2">decode_init/2</a></td><td></td></tr><tr><td valign="top"><a href="#encode-1">encode/1</a></td><td></td></tr><tr><td valign="top"><a href="#encode-2">encode/2</a></td><td></td></tr><tr><td valign="top"><a href="#encode-3">encode/3</a></td><td></td></tr><tr><td valign="top"><a href="#proto_driver-0">proto_driver/0</a></td><td></td></tr><tr><td valign="top"><a href="#proto_packet_type-0">proto_packet_type/0</a></td><td></td></tr><tr><td valign="top"><a href="#proto_vsn-0">proto_vsn/0</a></td><td></td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="atom_to_binary-1"></a>

###atom_to_binary/1##


`atom_to_binary(X) -> any()`

<a name="binary_to_atom-1"></a>

###binary_to_atom/1##


`binary_to_atom(X) -> any()`

<a name="binary_to_existing_atom-1"></a>

###binary_to_existing_atom/1##


`binary_to_existing_atom(X) -> any()`

<a name="contract_records-0"></a>

###contract_records/0##


`contract_records() -> any()`

<a name="decode-1"></a>

###decode/1##


<pre>decode(Input::binary()) -> <a href="#type-ok">ok()</a> | <a href="#type-error">error()</a> | <a href="#type-cont">cont()</a></pre>
<br></br>


<a name="decode-2"></a>

###decode/2##


<pre>decode(Input::binary(), Mod::module()) -> <a href="#type-ok">ok()</a> | <a href="#type-error">error()</a> | <a href="#type-cont">cont()</a></pre>
<br></br>


<a name="decode-3"></a>

###decode/3##


<pre>decode(Input::binary(), Mod::module(), X3::<a href="#type-cont">cont()</a>) -> <a href="#type-ok">ok()</a> | <a href="#type-error">error()</a> | <a href="#type-cont">cont()</a></pre>
<br></br>


<a name="decode_init-0"></a>

###decode_init/0##


<pre>decode_init() -> <a href="#type-cont">cont()</a></pre>
<br></br>


<a name="decode_init-1"></a>

###decode_init/1##


<pre>decode_init(Safe::boolean()) -> <a href="#type-cont">cont()</a></pre>
<br></br>


<a name="decode_init-2"></a>

###decode_init/2##


<pre>decode_init(Safe::boolean(), Input::binary()) -> <a href="#type-cont">cont()</a></pre>
<br></br>


<a name="encode-1"></a>

###encode/1##


<pre>encode(Input::term()) -&gt; iolist() | no_return()</pre>
<br></br>


<a name="encode-2"></a>

###encode/2##


<pre>encode(Input::term(), Mod::module()) -&gt; iolist() | no_return()</pre>
<br></br>


<a name="encode-3"></a>

###encode/3##


<pre>encode(Input::term(), Mod::module(), VNS::undefined | integer()) -&gt; iolist() | no_return()</pre>
<br></br>


<a name="proto_driver-0"></a>

###proto_driver/0##


`proto_driver() -> any()`

<a name="proto_packet_type-0"></a>

###proto_packet_type/0##


`proto_packet_type() -> any()`

<a name="proto_vsn-0"></a>

###proto_vsn/0##


`proto_vsn() -> any()`

