

#Module ubf_thrift_plugin#
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


Sample Thrift contract.



__Behaviours:__ [`ubf_plugin_stateless`](https://github.com/norton/ubf/blob/master/doc/plugin_stateless.md).<a name="description"></a>

##Description##


<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#description-0">description/0</a></td><td></td></tr><tr><td valign="top"><a href="#handlerEvent-1">handlerEvent/1</a></td><td></td></tr><tr><td valign="top"><a href="#handlerRpc-1">handlerRpc/1</a></td><td>rpc handler.</td></tr><tr><td valign="top"><a href="#handlerStart-1">handlerStart/1</a></td><td>start handler.</td></tr><tr><td valign="top"><a href="#handlerStop-3">handlerStop/3</a></td><td>stop handler.</td></tr><tr><td valign="top"><a href="#info-0">info/0</a></td><td></td></tr><tr><td valign="top"><a href="#keepalive-0">keepalive/0</a></td><td></td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="description-0"></a>

###description/0##




`description() -> any()`

<a name="handlerEvent-1"></a>

###handlerEvent/1##




`handlerEvent(Event) -> any()`

<a name="handlerRpc-1"></a>

###handlerRpc/1##




<pre>handlerRpc(Event::any()) -&gt; Reply::any()</pre>
<br></br>




rpc handler<a name="handlerStart-1"></a>

###handlerStart/1##




<pre>handlerStart(Args::[any()]) -&gt; {accept, Reply::any(), StateName::atom(), StateData::term()} | {reject, Reason::any()}</pre>
<br></br>




start handler<a name="handlerStop-3"></a>

###handlerStop/3##




<pre>handlerStop(Pid::pid(), Reason::any(), StateData::term()) -&gt; [void()](#type-void)</pre>
<br></br>




stop handler<a name="info-0"></a>

###info/0##




`info() -> any()`

<a name="keepalive-0"></a>

###keepalive/0##




`keepalive() -> any()`

