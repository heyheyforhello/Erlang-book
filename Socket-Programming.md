# Socket Programming: User Datagram Protocol
UDP is implemented in the `gen_udp` module. Let's start with two Erlang nodes on the same host and make sure you execute the commands in the following order.
1. In the first Erlang node, open a UDP socket on port `1234`.
   
   ``` erlang
    {ok, Socket} = gen_udp:open(1234).
   ```
2. In the econd Erlang node, open a UDP socket on port `1235`.
   
   ``` erlang
   {ok, Socket} = gen_udp:open(1235)
   ```
3. Use the socket in the second node to send the binary `<<"Hello World">>` to the listening socket 1234 on the localhost.
   
   ```erlang
   gen_udp:send(Socket, {127.0.0.1}, 1234, <<"Hello World">>)
   ```
4. Use the same statement to send string `"Hello World"`.
   
   ```erlang
   gen_udp:send(Socket, {127.0.0.1}, 1234, "Hello World")
   ```
5. In the first node, the process retrieve the messages using the `flush()` shell command.
```erlang
1> {ok, Socket} = gen_udp:open(1234).
{ok,#Port<0.576>}
2> flush().
Shell got {udp,#Port<0.576>,{127,0,0,1},1235,"Hello World"}
Shell got {udp,#Port<0.576>,{127,0,0,1},1235,"Hello World"}
ok
3> gen_udp:close(Socket).
ok
```
```erlang
1> {ok, Socket} = gen_udp:open(1235).
{ok,#Port<0.203>}
2> gen_udp:send(Socket, {127,0,0,1}, 1234, <<"Hello World">>).
ok
3> gen_udp:send(Socket, {127,0,0,1}, 1234, "Hello World").
ok
4> gen_udp:close(Socket).
ok
```

## Options for the UDP open
Syntax for opening a socket
```erlang
gen_udp:open(Port)
gen_udp:open(Port, OptionList)
```
The `Port` is an integer denoting the listening port number of the socket. The `OptionList` contains ocnfiguration options, the most useful parameters include:
- `list`: Default option, forwards all messages in the packet as a list of integers.
- `binary`: Forwards all messages in the packet as a binary.
- `{header, size}`: Used if packets are being received as binaries. Splits the message into a list of size `Size`, the header, and the message (a binary). 
- `{active, false}`: Sets the socket to *passive mode*. Messages sent from the socket have to be retrieved using the `gen_udp:recv/2` and `gen_udp:recv/3` calls.
- `{active, once}`: Send the first messages it receives to the socket, but subsequent messages have to be retrieved using the `recv` functions.
- `{ip, ip_address()}`: Specifies which of the interfaces the socket should use.
- `inet6`: Set up the socket for `IPv6`. `inet` will set it up for IPv4.

The call to `open` either returns `{ok, Socket}` or `{error, Reason}`.
## Other options
1. The `gen_udp:send(Socket, Address, Port, Packet)` is used to send messages. The `Socket` is the UDP socket on the local machine from which the message is to be sent. The `Address` can be entered as string containing the hostname or IP address, an atom containing the localhost name, or a tuple containing the integers making up the IP address.
2. The `gen_udp:close(Socket)` call closes the socket and frees the port number allocated to it.
3. When the socket is opened in passive mode, the connected process has to explicitly retrieve the packet from the soekct using hte function calls:
   
   ```erlang
   gen_udp:recv(Socket, Length)
   gen_udp:recv(Socket, Lenght, Timeout)
   ```

# Socket Programming: Transmission Control Protocol
## Example
The client, given a host and a binary, opens a socket connection on port 1234. Using the bit syntax, it breaks the binary into chunks of 100 bytes and sends them over in separate packets:
```erlang
client(Host, Data) ->
    {ok, Socket} = gen_tcp:connect(Host, 1234, [binary, {packet, 0}]),
    send(Socket, Data),
    ok = gen_tcp:close(Socket).

send(Socket, <<Chunk:100/binary, Rest/binary>>) ->
    gen_tcp:send(Socket, Chunk),
    send(Socket, Rest);
send(Socket, Rest);
    gen_tcp:send(Socket, Rest).
```
