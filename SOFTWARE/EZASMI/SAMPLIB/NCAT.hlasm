NCAT     TITLE 'TCP/IP EZASMI -- Assembler XF Interface Demo'
***********************************************************************
***                                                                 ***
*** Program:  NCAT                                                  ***
***                                                                 ***
*** Purpose:  Demonstrate using the EZASMI API to interface to      ***
***           the Hercules host's IP stack via the TCPIP (X'75')    ***
***           instruction.                                          ***
***                                                                 ***
***           NCAT communicates interactively with a network-cat    ***
***           tool like ncat, netcat, socat, or another NCAT        ***
***           instance. This can be used to perform interactive     ***
***           chats over the the internet, or even to run an        ***
***           interactive "telnet like" shell session.              ***
***                                                                 ***
***           A "telnet like" session requires a suitable shell to  ***
***           be used on the remote side, like for example bash     ***
***           on *i*x or powershell.exe on Windows systems. For     ***
***           example, on a Linux host, issuing the following       ***
***           command will provide a "telnet like" bash session:    ***
***                                                                 ***
***           ncat --exec "/bin/bash" -l 4466                       ***
***                                                                 ***
***           Issuing the NCAT command without arguments on TSO     ***
***           will then connect to this bash session, allowing to   ***
***           execute commands on the host directly from the TSO    ***
***           terminal.                                             ***
***                                                                 ***
*** Usage:    NCAT <options> <hostname> <port>                      ***
***                                                                 ***
***           NCAT operates in one of two modes: Connect mode and   ***
***           listen mode. In connect mode, NCAT works as a client. ***
***           In listen mode it is a server.                        ***
***                                                                 ***
***           - In connect mode, the <hostname> and <port>          ***
***             arguments tell where to connect to. <hostname> is   ***
***             required, and may be a hostname or an IP address.   ***
***             If <port> is supplied, it must be a decimal port    ***
***             number. If omitted, it defaults to 4466.            ***
***                                                                 ***
***           - In listen mode, <hostname> and <port> control the   ***
***             address the server will bind to. Both arguments are ***
***             optional. If <hostname> is omitted, it defaults to  ***
***             listening on all available addresses. If <port> is  ***
***             omitted, it defaults to 4466.                       ***
***                                                                 ***
***           Options                                               ***
***                                                                 ***
***           -l Bind and listen for incoming connections.          ***
***           -j Improved handling and readability for interactive  ***
***              chats. This option shouldn't be used with sessions ***
***              where the remote session partner is a shell style  ***
***              program.                                           ***
***                                                                 ***
***           Once the connection is established, data entered in   ***
***           the TSO session running NCAT will be transferred to   ***
***           the remote peer and data received from the remote     ***
***           peer will be displayed on the TSO terminal.           ***
***                                                                 ***
***           To terminate enter /* on the NCAT session, or send    ***
***           EOT from the remote peer, or close the connection at  ***
***           the remote peer. When NCAT is running as a server     ***
***           (i.e. the -l flag was specified) it is recommended    ***
***           to have the remote peer close its connection first,   ***
***           because otherwise the server port might enter a       ***
***           FIN_WAIT_x or a TIME_WAIT state, making it            ***
***           unavailable until the respective timeout(s)           ***
***           expire(s).                                            ***
***                                                                 ***
*** Note:     The NCAT program isn't designed to perform receive    ***
***           operations in quick succession, as, after each        ***
***           receive operation, the data received is written to    ***
***           the terminal before the next recv() call is           ***
***           performed. This is no (significant) restriction for   ***
***           interactive chatting or shell sessions. A peer        ***
***           program sending bulks of data in quick succession,    ***
***           however, can easily overrun NCAT.                     ***
***                                                                 ***
*** Updates:  2017/01/10 original implementation.                   ***
***           2017/01/22 command line processing using IKJPARSE.    ***
***                                                                 ***
*** Author:   Juergen Winkelmann, winkelmann@id.ethz.ch             ***
***                                                                 ***
*** Credits:  Thanks to Shelby Beach for providing an MVS 3.8j      ***
***           (Assembler XF) version of the EZASMI API.             ***
***                                                                 ***
***********************************************************************
TERMSIZE EQU   4096             size of terminal input buffer
RECVSIZE EQU   81920            size of network receive buffer
NCAT     CSECT ,                start of program
         STM   R14,R12,12(R13)  save registers
         LR    R12,R15          establish module addressability
         LA    R11,1(,R12)      second base ..
         LA    R11,4095(,R11)                 .. is base plus 4096
         USING NCAT,R12,R11     tell assembler of base
         ST    R13,REGSMAIN+4   chain ..
         LA    R2,REGSMAIN        .. the ..
         ST    R2,8(R13)            .. save ..
         LR    R13,R2                 .. areas
*
* Parse command line
*
         USING CPPL,R1          establish CPPL addressability
         MVC   PPLUPT,CPPLUPT   put in the UPT address from CPPL
         MVC   PPLECT,CPPLECT   put in the ECT address from CPPL
         MVC   PPLCBUF,CPPLCBUF put in the command buffer address
         DROP  R1               don't use CPPL any more
         CALLTSSR EP=IKJPARS,MF=(E,PPLUPT) invoke parse
         L     R2,ANSWER        get answer address
         USING OPERANDS,R2      establish answer addressability
         USING OPERPDE,R3       establish PDE addressability
         SR    R9,R9            exit indicator for storage release
         LA    R3,O1            get address of first operand PDE
         LA    R10,4            operand count
PROCESS  TM    OPERFLGS,X'80'   operand present?
         BZ    NEXT             no -> process next operand
         L     R4,OPER          yes -> get operand address
         LH    R5,OPERLNG       get operand length
         CHI   R5,2             at least two characters?
         BL    NOTFLAG          no -> can't be a flag
         CLC   0(2,R4),=CL2'-j' chat mode requested?
         BNE   *+12             no -> default to shell mode
         MVI   CHAT,X'01'       yes -> enable chat mode
         B     NEXT             process next operand
         CLC   0(2,R4),=CL2'-l' listen mode requested?
         BNE   *+12             no -> default to connect mode
         MVI   LISTEN,X'01'     yes -> enable listen mode
         B     NEXT             process next operand
         CLI   0(R4),C'-'       any other flag specified?
         BNE   NOTFLAG          no -> done with flags
         LA    R9,1             signal exit after storage release
         STRING 'Invalid flag specified: ',((R4),(R5)),                +
               INTO=PRTDATA     yes -> format error message ..
         TPUT  PRTDATA,L'PRTDATA       .. and tell user
         B     PARSERLS         release parse storage and exit
NOTFLAG  LR    R6,R4            operand address
         LR    R7,R5            operand length
ISDIGIT  CLI   0(R6),C'0'       EBCDIC zero or greater?
         BL    NOTPORT          no -> can't be a port number
         CLI   0(R6),C'9'       EBCDIC nine or lower?
         BH    NOTPORT          no -> can't be a port number
         LA    R6,1(,R6)        increment and ..
         BCT   R7,ISDIGIT                       .. check next character
         LR    R7,R5            number of digits entered as port number
         CHI   R7,5             more than five digits?
         BNH   *+8              no -> use it
         LA    R7,5             yes -> truncate to 5 digits
         ST    R7,PORTLEN       remember number of digits
         LA    R6,PORTR+5       right justify ..
         SR    R6,R7              .. to 5 digits
         BCTR  R7,0             decrement for EXecute
         EX    R7,MOVEPRTR      get port number right justified
         PACK  PORTD(8),PORTRPCK(10) pack port number and ..
         CVB   R6,PORTD             .. convert to binary
         STH   R6,PORT          remember port number
         B     NEXT             process next operand
NOTPORT  LR    R7,R5            operand length
         BCTR  R7,0             decrement for EXecute
         EX    R7,MOVEHOST      get hostname or IP address
         ST    R5,NAMELEN       remember length
NEXT     LA    R3,O2-O1(,R3)    address next operand
         BCT   R10,PROCESS      process next operand
         DROP  R2,R3            don't need PDEs any more
PARSERLS IKJRLSA (R2)           free storage that parse allocated
         LTR   R9,R9            exit signaled?
         BNZ   ALLDONE          yes -> exit
*
* Initialize EZASMI interface
*
         XC    EZASMTIE(TIELENTH),EZASMTIE clear EZASMI storage
         EZASMI TYPE=INITAPI,MAXSNO=MAXSNO,ERRNO=ERRCD,RETCODE=RETCD
*
* Try converting hostname/IP addr to network binary format using PTON
*
         EZASMI TYPE=PTON,AF='INET',SRCADDR=HOSTIN,SRCLEN=NAMELEN+2,   +
               DSTADDR=IPADDR,ERRNO=ERRCD,RETCODE=RETCD
         CLC   RETCD,=X'FFFFFFFF' conversion successful?
         BE    GETHBN           no, go treat input as a hostname
         LH    R6,NAMELEN+2     get length
         STH   R6,ADDRLEN       remember for display
         BCTR  R6,0             decrement for EXecute
         LA    R3,HOSTIN        address of IP address
         EX    R6,GETIP         copy IP address
*
* Resolve IP address to hostname using GETHOSTBYADDR
*
         EZASMI TYPE=GETHOSTBYADDR,HOSTADR=IPADDR,HOSTENT=HOSTENT,     +
               RETCODE=RETCD
         L     R3,HOSTENT       get hostname address ..
         L     R3,0(,R3)            .. from HOSTENT
         LR    R6,R3            \
CHKLEN   CLI   0(R6),X'00'       \
         BE    HOSTEND            \ compute length
         LA    R6,1(,R6)          / of hostname
         B     CHKLEN            /
HOSTEND  SR    R6,R3            /
         ST    R6,NAMELEN       remember length
         BCTR  R6,0             decrement for EXecute
         EX    R6,GETHOST       copy hostname
         B     SOCKET           create socket
*
* Resolve hostname to IP address using GETHOSTBYNAME
*
GETHBN   L     R6,NAMELEN       get length
         BCTR  R6,0             decrement for EXecute
         LA    R3,HOSTIN        hostname address
         EX    R6,GETHOST       copy hostname
         EZASMI TYPE=GETHOSTBYNAME,NAMELEN=NAMELEN,NAME=HOSTNAME,      +
               HOSTENT=HOSTENT,RETCODE=RETCD
         L     R3,HOSTENT       \
         L     R3,16(,R3)        \
         L     R3,0(,R3)          > get first IP address from HOSTENT
         L     R3,0(,R3)         /
         ST    R3,IPADDR        /
*
* Convert IP address to dotted decimal format using NTOP
*
         EZASMI TYPE=NTOP,AF='INET',SRCADDR=IPADDR,DSTADDR=ADDR,       +
               DSTLEN=ADDRLEN,ERRNO=ERRCD,RETCODE=RETCD
*
* Create socket
*
SOCKET   EZASMI TYPE=SOCKET,AF='INET',SOCTYPE='STREAM',                +
               ERRNO=ERRCD,RETCODE=SOCKDESC
         LT    R3,SOCKDESC      socket created?
         BNL   MODE             yes -> continue
         TPUT  MNOSOCK,L'MNOSOCK \ no -> tell user..
         B     RETNSOCK          /                  .. and exit
*
* listen or connect
*
MODE     CLI   LISTEN,X'01'     listen mode requested?
         BNE   CONNECT          no -> go connect
*
* Listen
*
         LH    R6,ADDRLEN       length of dotted decimal IP address
         ICM   R5,B'1111',NAMELEN length of hostname
         BNZ   *+16             not zero -> continue
         MVI   HOSTNAME,C'*'    \
         LA    R5,1              > else -> set hostname to asterisk
         ST    R5,NAMELEN       /
         STRING 'Listening at ',(HOSTNAME,(R5)),' (',(ADDR,(R6)),')',  +
               ' port ',(PORT,H,L),INTO=PRTDATA format message
BIND     EZASMI TYPE=BIND,S=SOCKDESC+2,NAME=SOCKADDR,                  +
               ERRNO=ERRCD,RETCODE=RETCD
         LT    R3,RETCD         socket bound?
         BNL   BOUND            yes -> continue
         CLFHSI ERRCD,EADDINUS  no -> address already in use?
         BE    RETRYBND               yes -> wait and retry
         TPUT  MNOBIND,L'MNOBIND      no -> tell user..
         B     RETURN           exit
RETRYBND TPUT  MINUSE,L'MINUSE  tell user
         STIMER WAIT,BINTVL=WAIT1000 wait ten seconds
         B     BIND             retry
BOUND    EZASMI TYPE=LISTEN,S=SOCKDESC+2,BACKLOG='1',                  +
               ERRNO=ERRCD,RETCODE=RETCD
         LT    R3,RETCD         listening?
         BNL   LISTNING         yes -> continue
         TPUT  MNOLIST,L'MNOLIST no -> tell user..
         B     RETURN           exit
LISTNING TPUT  PRTDATA,L'PRTDATA listening, tell user
         EZASMI TYPE=ACCEPT,S=SOCKDESC+2,NAME=SOCKADDR,                +
               ERRNO=ERRCD,RETCODE=NEWSOCK
         LT    R3,NEWSOCK       socket connected?
         BNL   ACCEPTED         yes -> continue
         TPUT  MNOACPT,L'MNOACPT no -> tell user..
         B     RETURN           exit
ACCEPTED EZASMI TYPE=CLOSE,S=SOCKDESC+2,ERRNO=ERRCD,RETCODE=RETCD
         MVC   SOCKDESC,NEWSOCK
         B     CONECTED         connection established
*
* Connect
*
CONNECT  CLC   IPADDR,=XL4'00000000' address zero?
         BNE   ADDROK           no -> continue
         MVC   IPADDR,=XL4'7F000001'  \
         MVC   ADDR,=CL9'127.0.0.1'    \
         MVC   HOSTNAME,=CL9'localhost' \ yes -> connect to
         LA    R6,9                     /        localhost (127.0.0.1)
         STH   R6,ADDRLEN              /
         ST    R6,NAMELEN             /
ADDROK   EZASMI TYPE=CONNECT,S=SOCKDESC+2,NAME=SOCKADDR,               +
               ERRNO=ERRCD,RETCODE=RETCD
         LT    R3,RETCD         socket connected?
         BNL   CONECTED         yes -> continue
         TPUT  MNOCONN,L'MNOCONN no -> tell user..
         B     RETURN           exit
*
* Identify peer
*
CONECTED EZASMI TYPE=GETPEERNAME,S=SOCKDESC+2,NAME=SOCKADDR,           +
               ERRNO=ERRCD,RETCODE=RETCD
         LT    R3,RETCD         GETPEERNAME successful?
         BNL   GOTPEER          yes -> continue
         TPUT  MNOPEER,L'MNOPEER no -> tell user..
         B     RETURN           exit
*
* Resolve peer IP address to hostname using GETHOSTBYADDR
*
GOTPEER  EZASMI TYPE=GETHOSTBYADDR,HOSTADR=IPADDR,HOSTENT=HOSTENT,     +
               RETCODE=RETCD
         L     R3,HOSTENT       get hostname address ..
         L     R3,0(,R3)            .. from HOSTENT
         LR    R6,R3            \
CHKLENP  CLI   0(R6),X'00'       \
         BE    HOSTENDP           \ compute length
         LA    R6,1(,R6)          / of hostname
         B     CHKLENP           /
HOSTENDP SR    R6,R3            /
         ST    R6,NAMELEN       remember length
         BCTR  R6,0             decrement for EXecute
         EX    R6,GETHOST       copy hostname
*
* Convert peer IP address to dotted decimal format using NTOP
*
         LA    R6,L'ADDR        reinitialize length ..
         STH    R6,ADDRLEN                            .. of IP address
         EZASMI TYPE=NTOP,AF='INET',SRCADDR=IPADDR,DSTADDR=ADDR,       +
               DSTLEN=ADDRLEN,ERRNO=ERRCD,RETCODE=RETCD
*
* Display connection message
*
         LH    R6,ADDRLEN       length of dotted decimal IP address
         L     R5,NAMELEN       length of hostname
         LA    R4,MFROM         indicate server mode
         LA    R3,L'MFROM       length of server mode message
         LT    R2,NEWSOCK       did we get a new socket?
         BNL   *+12             yes -> continue
         LA    R4,MTO           indicate client mode
         LA    R3,L'MTO         length of client mode message
         STRING ((R4),(R3)),(HOSTNAME,(R5)),' (',(ADDR,(R6)),')',      +
               ' port ',(PORT,H,L),INTO=PRTDATA format message
         TPUT  PRTDATA,L'PRTDATA display message
*
* Get storage for SEND/RECV buffers
*
         GETMAIN R,LV=TERMSIZE  get terminal input buffer
         ST    R1,TERMBUFA      remember address
         GETMAIN R,LV=RECVSIZE  get network input buffer
         ST    R1,NETIN         remember address
*
* Use linefeed character matching the installation's conversion tables
*
         LA    R3,1             convert ASCII linefeed ..
         ST    R3,NETLENR                .. to EBCDIC (LF or NL) and ..
         CALL  EZACIC05,(LF1+4,NETLENR),VL .. put it into ..
         MVC   LF2+1(1),LF1+4                .. immediate instructions
*
* Start terminal input task
*
         MVI   RUNNING,X'01'    indicate dialog is running
         IDENTIFY EP=NCATRMIN,ENTRY=GETLINE define entry point
         ATTACH EP=NCATRMIN     attach task
         ST    R1,TERMTCB       remember TCB
*
* While RUNNING is X'01' and no RECV error occurs:
* Read network input from peer and display it on terminal
*
         TPUT  UNLOCK,5,CONTROL unlock keyboard
READNET  CLI   RUNNING,X'01'    dialog running?
         BNE   RETURN           no -> exit
         MVC   NETLENR,NETINL   yes -> get length of network buffer
         EZASMI TYPE=RECV,S=SOCKDESC+2,NBYTE=NETLENR,BUF=*NETIN,       +
               ERRNO=ERRCD,RETCODE=RETCD
         LT    R3,RETCD         successfully received?
         BNL   RECVOK           yes -> continue
         CLFHSI ERRCD,ENOTSOCK  no -> socket already closed?
         BE    RETNSOCK               yes -> exit without error msg
         CLFHSI ERRCD,ECANCELD        no -> RECV cancelled?
         BE    RETNSOCK                     yes -> exit without msg
         TPUT  MNORECV,L'MNORECV            no -> tell user..
         B     RETURN           exit
RECVOK   BH    CONVERT          convert if at least one byte received
         TPUT  EOD,L'EOD        nothing received: Tell user ..
         B     RETURN              .. and exit
CONVERT  ST    R3,NETLENR       store length for use by EZACIC05
         L     R4,NETIN         address data
         CALL  EZACIC05,((R4),NETLENR),VL convert to EBCDIC
         CLI   0(R4),55         EOT character received?
         BNE   CHKCHATR         no -> check for dialog type
         TPUT  EOT,L'EOT        yes -> Tell user ..
         B     RETURN              .. and exit
CHKCHATR CLI   CHAT,X'01'       chat mode?
         BNE   DISPLAY          no -> display received data
         TPUT  NEWLINE,1,ASIS   yes -> avoid overtyping of input
*
* Split data at LF or CRLF and display line by line on terminal
*
DISPLAY  LR    R5,R4            initialize compare pointer
         SR    R8,R8            initialize to zero for CLIJ
NEXTCHAR IC    R8,0(,R5)        load character to check
LF1      CLIJE R8,X'0A',DISPLINE if linefeed -> display line
         LA    R5,1(,R5)                else -> point to next character
         B     NEXTCHAR         check next character
DISPLINE MVI   0(R5),X'15'      replace linefeed with newline
         LR    R7,R4            remember begin of current output line
         LA    R6,1(,R5)        remember begin of next output line
         BCTR  R5,0             last character of output line
         IC    R8,0(,R5)        load character to check
         CLIJNE R8,X'0D',LASTCHAR if not CR -> go ahead
         BCTR  R5,0                    else -> decrement
LASTCHAR LA    R5,1(,R5)        compute ...
         SR    R5,R4                       ... length of output line
         TPUT  (R7),(R5)        display on terminal
         LR    R5,R6            compute ..
         SR    R5,R7                      .. residual ..
         SR    R3,R5                                    .. count
         LR    R4,R6            initialize ..
         LR    R5,R6                         .. next line
         BNZ   NEXTCHAR         more data -> check it
         TPUT  NEWLINE,1,ASIS        else -> newline ..
         TPUT  PROMPT,1,ASIS                       .. prompt
         TPUT  UNLOCK,5,CONTROL                  .. and unlock keyboard
         B     READNET          continue reading socket
*
* Exit processing
*
RETURN   EZASMI TYPE=CLOSE,S=SOCKDESC+2,ERRNO=ERRCD,RETCODE=RETCD
RETNSOCK MVI   RUNNING,X'02'    terminate terminal task
         STIMER WAIT,BINTVL=WAIT50 wait half a second
         EZASMI TYPE=TERMAPI    terminate API
         CLI   RUNNING,X'00'    never running?
         BE    DONE             yes -> just exit
         STIMER WAIT,BINTVL=WAIT50 wait half a second
         CLI   RUNNING,X'FF'    terminal subtask already terminated?
         BE    DONE             yes -> we are done
         STIMER WAIT,BINTVL=WAIT50 no -> wait half a second and exit
*
* Release SEND/RECV buffers
*
DONE     ICM   R1,B'1111',TERMBUFA send buffer address
         BZ    RELNETIN         skip release if never allocated
         FREEMAIN R,LV=TERMSIZE,A=(R1) release send buffer
RELNETIN ICM   R1,B'1111',NETIN receive buffer address
         BZ    ALLDONE          skip release if never allocated
         FREEMAIN R,LV=RECVSIZE,A=(R1) release receive buffer
*
* That's all folks!
*
ALLDONE  L     R13,4(,R13)      caller's save area pointer
         RETURN (14,12),RC=0    restore registers and return
*
* Read terminal input and send it to peer
*
         DROP  R11,R12          avoid confusion
MAINLEN  DC    Y(GETLINE-NCAT)  length of mainline code
GETLINE  STM   R14,R12,12(R13)  save registers
         USING GETLINE,R15      tell assembler of temporary base
         ST    R13,REGSTGET+4   chain ..
         LA    R2,REGSTGET        .. the ..
         ST    R2,8(R13)            .. save ..
         LR    R13,R2                 .. areas
         LA    R5,GETLINE       address ..
         BCTR  R5,0                      .. length of ..
         BCTR  R5,0                                    .. mainline code
         DROP  R15              temporary base no longer needed
         LR    R12,R15          entry address minus mainline ..
         SH    R12,0(,R5)                    .. length is mainline base
         LA    R11,1(,R12)      second base ..
         LA    R11,4095(,R11)                 .. is base plus 4096
         USING NCAT,R12,R11     tell assembler to use mainline bases
*
* Loop while RUNNING is X'01' and no SEND error occurs
*
         TPUT  NEWLINE,1,ASIS   initial newline and ..
         TPUT  PROMPT,1,ASIS                          .. prompt
         B     READNEXT         unlock keyboard and read from terminal
READTERM CLI   RUNNING,X'01'    dialog running?
         BNE   STOPTRMI         no -> stop this task
         L     R9,TERMBUFA      address of terminal buffer
         L     R8,TERMBUFS      size of terminal buffer
         TGET  (R9),(R8),,NOWAIT yes -> read input from terminal
         CFI   R15,4            input available?
         BNE   GOTINPUT         yes -> check for EOD
         STIMER WAIT,BINTVL=WAIT25 no -> wait quarter second ..
         B     READTERM                  .. and try again
GOTINPUT CLHHSI 0(R9),C'/*'     EOD?
         BNE   SENDIT           no -> send it
         TPUT  EOD,L'EOD        yes -> Tell user ..
         B     STOPTRMI                .. and stop this task
SENDIT   LA    R3,1(,R1)        increment length for newline
         ST    R3,NETLENS       store length for use by EZACIC04/SEND
         LA    R2,0(R1,R9)      terminal input
LF2      MVI   0(R2),*-*        insert linefeed character
         L     R4,TERMBUFA      address data
         CALL  EZACIC04,((R4),NETLENS),VL convert to ASCII
         EZASMI TYPE=SEND,S=SOCKDESC+2,NBYTE=NETLENS,BUF=*TERMBUFA,    +
               ERRNO=ERRCDS,RETCODE=RETCDS,MF=(E,TRMPLIST)
         LT    R3,RETCDS        successfully sent?
         BNL   CHKCHATS         yes -> check for dialog type
         TPUT  MNOSEND,L'MNOSEND no -> tell user..
         B     STOPTRMI                .. and stop this task
CHKCHATS CLI   CHAT,X'01'       chat mode?
         BNE   READNEXT         no -> read next line
         TPUT  NEWLINE,1,ASIS   yes -> newline and ..
         TPUT  PROMPT,1,ASIS                         .. prompt
READNEXT TPUT  UNLOCK,5,CONTROL unlock keyboard
         B     READTERM         start over
*
* Close connection and exit terminal input handler
*
STOPTRMI CLI   RUNNING,X'02'    stopped by main task?
         BE    OUT              yes -> exit, no -> close, then exit
         EZASMI TYPE=CLOSE,S=SOCKDESC+2,ERRNO=ERRCDS,RETCODE=RETCDS,   +
               MF=(E,TRMPLIST)
         STIMER WAIT,BINTVL=WAIT25 wait quarter second
OUT      MVI   RUNNING,X'FF'    indicate we are out
         L     R13,4(,R13)      caller's save area pointer
         RETURN (14,12),RC=0    restore registers and return
*
* EXecuted instructions
*
GETHOST  MVC   HOSTNAME(*-*),0(R3) copy hostname
GETIP    MVC   ADDR(*-*),0(R3)  copy IP address
MOVEPRTR MVC   0(*-*,R6),0(R4)  right justify port number
MOVEHOST MVC   HOSTIN(*-*),0(R4) get hostname/IP addr from command line
*
* Data areas
*
REGSMAIN DS    18F              main task save area
REGSTGET DS    18F              terminal input task save area
ERRCD    DC    A(*-*)           error code main task
ERRCDS   DC    A(*-*)           error code terminal input task
RETCD    DC    A(*-*)           return code main task
RETCDS   DC    A(*-*)           return code terminal input task
MAXSNO   DC    A(*-*)           highest socket number assigned
SOCKDESC DC    F'-1'            socket descriptor
SOCKADDR DS    0F               sockaddr structure
AF@INET  DC    H'2'             address family
PORT     DC    H'4466'          port to connect to or listen at
IPADDR   DC    F'0'             INET address (netid)
         DC    XL8'00'          Reserved area not used
NEWSOCK  DC    F'-1'            new socket descriptor from ACCEPT
HOSTENT  DC    F'0'             address of HOSTENT structure goes here
PORTD    DC    D'0'             decimal port number
MNOSOCK  DC    C'SOCKET failed'
MNOCLOS  DC    C'CLOSE failed'
MNOCONN  DC    C'CONNECT failed'
MNOBIND  DC    C'BIND failed'
MINUSE   DC    C'BIND: Address in use, retrying in 10 seconds...'
MNOLIST  DC    C'LISTEN failed'
MNOACPT  DC    C'ACCEPT failed'
MNOPEER  DC    C'GETPEERNAME failed'
MNOSEND  DC    C'SEND failed'
MNORECV  DC    C'RECV failed'
MFROM    DC    C'Connection from '
MTO      DC    C'Connected to '
EOD      DC    C'Terminating at EOD'
EOT      DC    C'Terminating at EOT'
NEWLINE  DC    X'15'            EBCDIC newline character
PROMPT   DC    C'$'             prompt character
CHAT     DC    X'00'            dialog type: chat = 1, shell = 0
LISTEN   DC    X'00'            mode: listen = 1, connect = 0
RUNNING  DC    X'00'            status indicator
UNLOCK   DC    X'27F1C30013'    control datastream to unlock keyboard
ADDR     DC    CL15' '          dotted decimal IP address
ADDRLEN  DC    Y(L'ADDR)        length of dotted decimal IP address
HOSTNAME DC    CL256' '         Hostname
TERMTCB  DS    F                TCB of terminal input subtask
WAIT25   DC    F'25'            quarter of a second
WAIT50   DC    F'50'            half a second
WAIT1000 DC    F'1000'          ten seconds
NAMELEN  DC    F'0'             length of hostname
PORTLEN  DC    F'0'             length of port number
NETLENR  DS    F                number of bytes for RECV/EZACIC05
NETLENS  DS    F                number of bytes for SEND/EZACIC04
PORTRPCK DC    CL5' '           port right justified for PACK
PORTR    DC    CL5' '           port right justified
PRTDATA  DC    CL78' '          formatted result
HOSTIN   DC    CL50' '          hostname or IP addr from command line
TERMBUFA DC    F'0'             address of terminal input buffer
TERMBUFS DC    A(TERMSIZE)      size of terminal input buffer
NETIN    DC    F'0'             address of network input buffer
NETINL   DC    A(RECVSIZE)      size of network input buffer
ANSWER   DS    F                IKJPARSE answer place
PPLUPT   DS    A                address of UPT
PPLECT   DS    A                address of ECT
PPLECB   DC    A(ECB)           address of CP's ECB
PPLPCL   DC    A(PCLDEFS)       address of PCL
PPLANS   DC    A(ANSWER)        address of answer place
PPLCBUF  DS    A                address of command buffer
PPLUWA   DC    A(0)             address of user work area
ECB      DC    F'0'             CP's event control block
PCLDEFS  IKJPARM  DSECT=OPERANDS begin of operand descriptions
O1       IKJIDENT 'Operand 1',ASIS,CHAR
O2       IKJIDENT 'Operand 2',ASIS,CHAR
O3       IKJIDENT 'Operand 3',ASIS,CHAR
O4       IKJIDENT 'Operand 4',ASIS,CHAR
         IKJENDP ,              end of operand descriptions
TRMPLIST EZASMI MF=L            EZASMI parameter list for terminal task
         EZASMI TYPE=TASK,STORAGE=CSECT EZASMI task storage
         LTORG                  literals go here
         STRING GENERATE        STRING storage and code goes here
ENOTSOCK EQU   38               socket already closed on RECV
EADDINUS EQU   48               address already in use
ECANCELD EQU   1009             RECV cancelled, EIBMCANCELLED
OPERPDE  DSECT ,                PDE mapping for a positional operand
OPER     DS    F                address of operand value
OPERLNG  DS    H                length of operand value
OPERFLGS DS    CL1              flags byte
         DS    CL1              reserved
         IKJCPPL ,              command processor parameter list
         CVT   DSECT=YES        CVT mapping needed for CALLTSSR
         YREGS ,                register equates
         END   NCAT             end of program
