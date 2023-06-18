      $SET SERIAL REENTRANT(2) NOCANCEL SOURCEFORMAT(VARIABLE)
      $SET SQL(NOSMFTRACE BEHAVIOR=MAINFRAME)
      $SET CONSTANT XABUILD"ODBC" CONSTANT LKSWITCH"BASIC"
      $SET CONSTANT MFDBFH-SUPPORT"1"
      *>$SET CONSTANT DEBUG-PING"1" 
       *>
       *> Micro Focus XA one-phase commit ODBC switch module (Windows).
       *>
       *> Copyright (C) Micro Focus 2007-2019. All Rights Reserved.
       *> 

       *> The Open String for this module is defined as :
       *> DSN=mydsn[,USRPASS=userid.passwd]
       *> [,USERP=(T|F)][,BATCHONLY=(T|F)][,LNAME=(T|F)]
       *> [,ISOLEVEL=(CR|RR|SZ|UR)]
       *>
       *> where :
       *>
       *> mydsn    : ODBC Data Source Name, as defined within the ODBC
       *>            Administrator utility.
       *> userid   : User ID required to connect to the data source
       *> passwd   : Password required for User userid to connect to the
       *>            data source
       *> BATCHONLY: When F the connection is only available for JCL jobs
       *>            Default T.
       *> LNAME    : When 'T' long names are used. Default 'F' meaning
       *>            short names will be used.
       *> ESLOGGING: May be used to enable customized logging
       *>            The value is a 16-bit mask expressed as exactly 
       *>            four hex digits.
       *>            0001 enables logging related to connection failures
       *>            and invalid open string keywords for JCL jobs
       *>            (these errors are always logged for CICS connections)
       *> USERP    : When T the connection will switch to the current ES
       *>            user for a CICS session or JCL job. Default F.
       *>            This keyword is supported for SQL Server only.
       *> ISOLEVEL : Transaction isolation levels.
       *>                 CR - Committed Read (Default)
       *>                 RR - Repeatable Read
       *>                 SZ - Serializable
       *>                 UR - Uncommitted Read
       *>
       *> This module may be customized to avoid using credentials from
       *> the open string configured with Enterprise Server, or to 
       *> override the entire open string. Search for CUSTOMIZE and 
       *> modify the Customization SECTION 

       IDENTIFICATION DIVISION.
      $IF XAPLATFORM = "WINDOWS"
      $IF DYNAMICREGISTRATION = "Y"
       PROGRAM-ID. esodbcxa_d.
      $ELSE
       PROGRAM-ID. esodbcxa.
      $END
      $ELSE
      $IF P64 SET
      $IF DYNAMICREGISTRATION = "Y"
       PROGRAM-ID. ESODBCXA64_D.
      $ELSE
       PROGRAM-ID. ESODBCXA64.
      $END
      $ELSE
      $IF DYNAMICREGISTRATION = "Y"
       PROGRAM-ID. ESODBCXA_D.
      $ELSE
       PROGRAM-ID. ESODBCXA.
      $END
      $END
      $END       
       ENVIRONMENT DIVISION.

       SPECIAL-NAMES.
      $IF XAPLATFORM = "WINDOWS"
          CALL-CONVENTION 74  is odbcapi
          CALL-CONVENTION 0 is cdecl.
      $ELSE
          CALL-CONVENTION 0 is odbcapi
          CALL-CONVENTION 0 is cdecl.
      $END

       DATA DIVISION.

       WORKING-STORAGE SECTION.

       78 MODULENAME value "ODBC".

      $IF DEBUG-PING DEFINED
       *> for debuggng
       01  pingCount           pic s9(9) comp-5.
       78  pingFailInterval    value 2.
       *> SQL Server and PostgreSQL       
       01  SPID                pic 9(9).
       *> SQL Server     
       01  ssdb                pic x(32) value "northwindpw".
       01  ssuid               pic x(32) value "sa".
       01  sspwd               pic x(32) value "Wibble1234".
       *>PostgreSQL
       01  pgdb                pic x(32) value "PostgresANSI".
       01  pguid               pic x(32) value "postgres".
       01  pgpwd               pic x(32) value "Wibble1234".
       01  pgStatus            binary-long.
       *>DB2
       01  db2db               pic x(32) value "LOOP32SAMP".
       01  db2uid              pic x(32) value "ChrisLe".
       01  db2pwd              pic x(32) value "Monday99".
       01  AH                  pic S9(18) comp-5.
       01  AH-CHAR             pic s9(18).
      *>Oracle
       01  oradb               pic x(32) value "ORCL".
       01  orauid              pic x(32) value "SYSTEM".
       01  orapwd              pic x(32) value "MANAGER".
       01  SID     pic 9(9).
       01  Serial  pic 9(9).
      $END

      $IF DYNAMICREGISTRATION = "Y"
       78 esxa-ep-prefix                 VALUE "ES1D".
       78 esxa-ep2-prefix                VALUE "ES2D".
      $ELSE
       78 esxa-ep-prefix                 VALUE "ES1S".
       78 esxa-ep2-prefix                VALUE "ES2S".
      $END

          78 esxa-open 
                  VALUE esxa-ep-prefix & "ESODBCXA_OPEN".
	      78 esxa-2pc-open 
                  VALUE esxa-ep2-prefix & "ESODBCXA_OPEN".
          78 esxa-close 
                  VALUE esxa-ep-prefix & "ESODBCXA_CLOSE".
          78 esxa-2pc-close 
                  VALUE esxa-ep2-prefix & "ESODBCXA_CLOSE".
          78 esxa-start 
                  VALUE esxa-ep-prefix & "ESODBCXA_START".
          78 esxa-end 
                  VALUE esxa-ep-prefix & "ESODBCXA_END".
          78 esxa-rollback 
                  VALUE esxa-ep-prefix & "ESODBCXA_ROLLBACK".
          78 esxa-2pc-rollback 
                  VALUE esxa-ep2-prefix & "ESODBCXA_ROLLBACK".
          78 esxa-prepare 
                  VALUE esxa-ep-prefix & "ESODBCXA_PREPARE".
          78 esxa-commit 
                  VALUE esxa-ep-prefix & "ESODBCXA_COMMIT".
          78 esxa-2pc-commit 
                  VALUE esxa-ep2-prefix & "ESODBCXA_COMMIT".
          78 esxa-recover 
                  VALUE esxa-ep-prefix & "ESODBCXA_RECOVER".
          78 esxa-forget 
                  VALUE esxa-ep-prefix & "ESODBCXA_FORGET".
          78 esxa-complete 
                  VALUE esxa-ep-prefix & "ESODBCXA_COMPLETE".
          78 esxa-ping
                  VALUE esxa-ep-prefix & "ESODBCXA_PING".
          78 esxa-register
                  VALUE esxa-ep-prefix & "ESODBCXA_REGISTER".

       78 XAModuleName                     VALUE "ESODBCXA".

       78 78-XA-ONEPHASE-SUPPORT           VALUE H'80000000'.

       *> Variables for extracting the DSN, UserID and Password from
       *> the Open String passed from the Enterprise Server
       *> Configuration page.

       01 TmpWorkString                    PIC X(256).
       01 ParamString                      PIC X(256).
       01 ValueString                      PIC X(256).

       *> Parameters for ODBC API calls.

       01 NullPtr                          POINTER VALUE NULL.
       78 SqlAttrConnectionDead            VALUE 1209.
       78 SqlCoptSSConnectionDead          VALUE 1244.
       78 SqlAttrPingDb                    VALUE 2545.
       78 SqlTxnIsolation                  VALUE 108.
       78 SqlCdFalse                       VALUE 0.
       78 SqlDriverVer                     VALUE 7.
       01 SqlRtn                           PIC S9(4) COMP-5.
       01 ws-connection-dead               PIC 9(9) COMP-5.
       *> According to IBM support incident the ping counter is SQLLEN not SQLINTEGER
      $IF P64 SET
       01 Db2PingTime             PIC S9(18) COMP-5.
      $ELSE
       01 Db2PingTime             PIC S9(9) COMP-5.
      $END
       01 SqlHandleDBC                     PIC 9(4) COMP-5 VALUE 2.
       01 SqlRtnSize                       PIC S9(9) COMP-5.


       EXEC SQL INCLUDE SQLCA END-EXEC.

       EXEC SQL BEGIN DECLARE SECTION END-EXEC

       *> SQL Connect information, derived from the Open string
       *> as defined within the Enterprise Server configuration.

       01 ConnectionStructure.
          03 DsnString                     PIC X(32) VALUE LOW-VALUES.
          03 UsrPassString                 PIC X(256) VALUE LOW-VALUES.
          03 ThisConnName                  PIC X(32) VALUE LOW-VALUES.
      $IF P64 SET
          03 ThisConnHandle                PIC X(8) COMP-5.
      $ELSE
          03 ThisConnHandle                PIC X(4) COMP-5.
      $END


       *> Array of connection details.
       78  MAX-CONNECTIONS VALUE 20.

       01 ConnInfo OCCURS MAX-CONNECTIONS.
          03 ResMgrID                      PIC S9(9) COMP-5.
          03 ResName                       PIC X(8) VALUE SPACES.
          03 ConnectionName                PIC X(32).
          03 LastCall                      PIC X(8).
      $IF P64 SET
          03 ConnectionHandle              PIC X(8) COMP-5.
      $ELSE
          03 ConnectionHandle              PIC X(4) COMP-5.
      $END
          03 DatabaseType                  PIC X(10).
          03 ConnectionString              PIC X(256) VALUE LOW-VALUES.
          03 DoImpersonate.
             05 DoBATCH                    PIC X COMP-X.
             05 DoCICS                     PIC X COMP-X.
             05 DoIMS                      PIC X COMP-X.
             05 DoORACLE                   PIC X COMP-X.
          03 LUsername                     PIC X COMP-X VALUE 0.
          03 TxEnabled                     PIC X COMP-X VALUE 0.
          03 EsLoggingLevel                PIC X(2) COMP-x.
          03 Impersonating                 PIC X COMP-X VALUE 0.
          03 SqlTxnIsolationValue          PIC 9(9) COMP-5 VALUE 2.
             88 SqlIsoReadUncommitted         VALUE  1.
             88 SqlIsoReadCommitted           VALUE  2.
             88 SqlIsoRepeatableRead          VALUE  4.
             88 SqlIsoSerializable            VALUE  8.
             88 SqlIsoDefault                 VALUE  1024.
      $if MFDBFH-SUPPORT defined
          03 MfdbfhRegistered              PIC X COMP-X VALUE 0.
      $end MFDBFH-SUPPORT defined
          03 DriverVersion.
             05 MajorVer                   PIC X(2).
             05 filler                     PIC X(1).
             05 MinorVer                   PIC X(2).
             05 filler                     PIC X(1).
             05 ReleaseVer                 PIC X(4).
             05 filler                     PIC X(1).

       01 MFSQLMESSAGETEXT                 PIC X(400).

       EXEC SQL END   DECLARE SECTION END-EXEC

       *> Variables for tracking connections used.

       01 sqltext                          PIC X(300).

      $IF P64 SET
       01 hvXaTrigger                      PIC S9(18) COMP-5 VALUE -4.
      $ELSE
       01 hvXaTrigger                      PIC S9(8) COMP-5 VALUE -4.
      $END
       01 hvppTrigger REDEFINES hvXaTrigger PROCEDURE-POINTER.

       01 pp                               PROCEDURE-POINTER.

       01 ix                               PIC S9(4) COMP-5.

       01 ImpersonatedUser                 PIC X(32).
       01 ProxyUserString                  PIC X(128).
       01 ProxyPassString                  PIC X(128).
       01 NewUsrPassString                 PIC X(256).
       01 NewLen                           PIC S9(4) COMP-5.

       01 ESXASuppPtr                      USAGE PROCEDURE-POINTER
                                                 VALUE NULL.

       *> Support for tracing

       01 XID.
      $IF P64 SET
      $    IF XAPLATFORM = "WINDOWS"
           03 lk-xid-formatid              PIC S9(9) COMP-5.
           03 lk-xid-gtrid-length          PIC S9(9) COMP-5.
           03 lk-xid-bqual-length          PIC S9(9) COMP-5.
      $    ELSE
           03 lk-xid-formatid              PIC S9(18) COMP-5.
           03 lk-xid-gtrid-length          PIC S9(18) COMP-5.
           03 lk-xid-bqual-length          PIC S9(18) COMP-5.
      $    END-IF
      $ELSE
           03 lk-xid-formatid              PIC S9(9) COMP-5.
           03 lk-xid-gtrid-length          PIC S9(9) COMP-5.
           03 lk-xid-bqual-length          PIC S9(9) COMP-5.
      $END
           03 lk-xid-value                 PIC X(128).

       01  savedConnectionName             PIC X(32) VALUE SPACES.
       01  connectionCount                 PIC S9(4) COMP-5 VALUE 0. 
       01  errorCode                       PIC S9(9).
       01  curDriverVer                    PIC 9(2) VALUE 0.

      $IF XA-RECONNECT DEFINED
       01  ws-env-var                  pic x(20).
       01  ws-conn-retry               pic s9(9) comp-5.
       01  ws-retry-time               pic 9(09) comp-5.
      $  IF P64 SET
       01  ws-sleep-time               pic x(08) comp-5.
      $  ELSE
       01  ws-sleep-time               pic x(04) comp-5.
      $  END
      $END

       COPY "cbltypes.cpy".

       COPY "mfctf.cpy".

       COPY "xacustom.cpy".

       COPY "xaws.cpy" REPLACING =='(XXXXX)'== BY =='odbc'==.

       PROCEDURE DIVISION.
       
           IF isInitialised = 0
               PERFORM VARYING ConnIX FROM 1 BY 1
                       UNTIL ConnIX > MAX-CONNECTIONS
                   MOVE 0 TO ResMgrID(ConnIX)
                   MOVE SPACES TO ResName(ConnIX)
                   MOVE SPACES TO ConnectionName(ConnIX)
               END-PERFORM

      *> Try to load Customization Exit routine
      $IF XAPLATFORM = "WINDOWS"
               SET CustomizationExitRoutine TO ENTRY "ESXAEXTCFG.dll"
      $ELSE
      $   IF P64 SET
               SET CustomizationExitRoutine TO ENTRY "ESXAEXTCFG64.so"
      $   ELSE
               SET CustomizationExitRoutine TO ENTRY "ESXAEXTCFG.so"
      $   END
      $END

               MOVE 1 TO isInitialised

      $IF DEBUG-PING DEFINED
               move 0 to pingCount
      $END 

               *> Set up internal XA structure to point to internal
               *> ENTRY points of this program.

      $IF XAPLATFORM = "WINDOWS"
               SET ESXASuppPtr TO ENTRY "esxasupp.dll"
      $END

           IF tracingInitialised = 0
              PERFORM initialise-odbc-tracing
              MOVE 1 TO tracingInitialised
           END-IF

      $if MFDBFH-SUPPORT defined
               perform mfdbfh-initialise
      $end MFDBFH-SUPPORT defined
           END-IF

      $IF XA-RECONNECT DEFINED
           *> Allow for a number of retry attempts based on
           *> enviroonment variable XA_RECONN_RETRIES
           move spaces to ws-env-var
           display "XA_RECONN_RETRIES" UPON ENVIRONMENT-NAME
           accept ws-env-var from environment-value
           if ws-env-var = spaces
               move 5 to ws-conn-retry
           else
               move function numval(ws-env-var) to ws-conn-retry
               if ws-conn-retry <= 0
                   move 5 to ws-conn-retry
               end-if
           end-if
           *> Determine number of seconds to wait between retries
           *> based on XA_RECONN_DELAY
           move spaces to ws-env-var
           display "XA_RECONN_DELAY" UPON ENVIRONMENT-NAME
           accept ws-env-var from environment-value
           if ws-env-var = spaces
               move 5 to ws-retry-time
           else
               move function numval(ws-env-var) to ws-retry-time
               if ws-retry-time <= 0
                   move 5 to ws-retry-time
               end-if
           end-if
           compute ws-sleep-time = (ws-retry-time * 1000)
      $END

           *> Set up internal XA structure to point to internal
           *> ENTRY points of this program.

           INITIALIZE mfesxa-switch-struct


           MOVE 'Micro Focus ODBC XA struct' & esxa-ep-prefix & x'00'
                TO mfesxa-RM-name
      $IF DYNAMICREGISTRATION = "Y"
      $IF XAPLATFORM = "WINDOWS"
           SET pp TO ENTRY "casaxlib.dll"
      $END
           MOVE TMREGISTER                 TO mfesxa-flags
      $ELSE
           MOVE TMNOFLAGS                  TO mfesxa-flags
      $END
           MOVE 0                          TO mfesxa-version  
           SET mfesxa-open-entry           TO ENTRY esxa-2pc-open
           SET mfesxa-close-entry          TO ENTRY esxa-2pc-close
           SET mfesxa-start-entry          TO ENTRY esxa-start
           SET mfesxa-end-entry            TO ENTRY esxa-end
           SET mfesxa-rollback-entry       TO ENTRY esxa-2pc-rollback
           SET mfesxa-prepare-entry        TO ENTRY esxa-prepare
           SET mfesxa-commit-entry         TO ENTRY esxa-2pc-commit
           SET mfesxa-recover-entry        TO ENTRY esxa-recover
           SET mfesxa-forget-entry         TO ENTRY esxa-forget
           SET mfesxa-complete-entry       TO ENTRY esxa-complete
           SET mfesxa-ping-entry           TO ENTRY esxa-ping

           SET mfesxa-1phase-commit-entry     TO ENTRY esxa-commit
           SET mfesxa-1phase-rollback-entry   TO ENTRY esxa-rollback
           SET mfesxa-1phase-connect-entry    TO ENTRY esxa-open
           SET mfesxa-1phase-disconnect-entry TO ENTRY esxa-close
           COMPUTE mfesxa-flags = 
                       mfesxa-flags B-OR 78-XA-ONEPHASE-SUPPORT

           IF tracingInitialised NOT = 1
           PERFORM initialise-odbc-tracing
           MOVE 1 TO tracingInitialised
           END-IF

           IF 78-CTF-FLAG-LEVEL-INFO >= ctf-trace-level
               MOVE "ESODBCXA Initialized" TO ls-trace-data-desc
               MOVE EVENT-ODBC-INIT TO ls-trace-event
               MOVE 78-CTF-FLAG-LEVEL-INFO TO ls-trace-level
               PERFORM trace-odbc-event
           END-IF

           GOBACK RETURNING ADDRESS OF mfesxa-switch-struct
           .
      *--------------------------------------------------------------*
	  
       xa-2pc-open SECTION.
       ENTRY esxa-2pc-open USING BY REFERENCE lk-open-string
                                 BY VALUE     lk-rmid
                                 BY VALUE     lk-flags.

           MOVE lk-rmid TO ws-rmid

        *> Initialise tracing

           IF tracingInitialised = 0
              PERFORM initialise-odbc-tracing
              MOVE 1 TO tracingInitialised
           END-IF

           *> Get Recovery guid, region and XA resource names
           *> and the account the SEP is running in
           *> For 1-phase we only need the initial account
           *> but the other properties can be used in diagnostics
           MOVE 2 TO XAi-version
           CALL "mfxaGetRegionInfo" USING BY REFERENCE XA-inquiry
           END-CALL

           IF XAi-SEP-initial-user = LOW-VALUES
               MOVE SPACES TO XAi-SEP-initial-user
           END-IF

           INSPECT XAi-XA-Resource-Name
               REPLACING ALL low-values BY SPACE

           *> Find an empty slot in the connections table
           PERFORM VARYING ConnIX FROM 1 BY 1
                UNTIL ConnIX > MAX-CONNECTIONS
                OR ResMgrID(ConnIX) = 0
           END-PERFORM

           *> Initialize user personaliation and CICS enablement
           *> to default values, ie CICS/Web services enabled
           *> but no user personalization
           MOVE 1 TO TxEnabled(ConnIX)
           MOVE 1 TO LUsername(ConnIX)
           INITIALIZE DoImpersonate(ConnIX)
           MOVE 0 TO Impersonating(ConnIX)
           MOVE 0 TO EsLoggingLevel(ConnIX)
           SET SqlIsoDefault(ConnIX) to TRUE

           IF CustomizationExitRoutine not = null
               *> Get external configuration info
               PERFORM GetCustomizationParams
           END-IF

           PERFORM ParseOpenString

           *> BATCHONLY=T, skip all 2pc tasks.
           IF TxEnabled(ConnIX) = 0
               MOVE lk-rmid TO ResMgrID(ConnIX)
               GOBACK RETURNING XA-OK
           END-IF
		   
           call esxa-open using
                by reference lk-open-string
                by value     lk-rmid
                by value     lk-flags
                returning    ws-open-rc
           end-call
		   
           *> Discard credentials as soon as they are no longer needed
           INITIALIZE UsrPassString
           initialize password

           GOBACK RETURNING ws-open-rc
           .
		   
       xa-open SECTION.
       ENTRY esxa-open USING BY REFERENCE lk-open-string
                                 BY VALUE     lk-rmid
                                 BY VALUE     lk-flags.

           MOVE lk-rmid TO ws-rmid

        *> Initialise tracing

           IF tracingInitialised = 0
              PERFORM initialise-odbc-tracing
              MOVE 1 TO tracingInitialised
           END-IF

           *> Get Recovery guid, region and XA resource names
           *> and the account the SEP is running in
           *> For 1-phase we only need the initial account
           *> but the other properties can be used in diagnostics
           MOVE 2 TO XAi-version
           CALL "mfxaGetRegionInfo" USING BY REFERENCE XA-inquiry
           END-CALL

           IF XAi-SEP-initial-user = LOW-VALUES
               MOVE SPACES TO XAi-SEP-initial-user
           END-IF

           INSPECT XAi-XA-Resource-Name
               REPLACING ALL low-values BY SPACE

           MOVE 0 TO ThisConnHandle
           EXEC SQL
                   GET HDBC INTO :ThisConnHandle
           END-EXEC

           IF SQLCODE = 0
               *> Opened connection exists. Check to make sure RMID of the opened connection
               *> is the same of the current RMID.
               PERFORM VARYING ConnIX FROM 1 BY 1
                 UNTIL ConnIX > MAX-CONNECTIONS OR
                 (ResMgrID(ConnIX) = lk-rmid AND 
                  ConnectionHandle(ConnIX) = ThisConnHandle)
               END-PERFORM
               IF ConnIX NOT > MAX-CONNECTIONS
                   *> Matched opened connection exists, so reuse it. Return without error.
                   MOVE 0 TO ws-open-rc
                   IF XAi-SEP-type = sepIsJCL
                       *> For 1-phase TX jobs we need to do this later
                       PERFORM BeginUserImpersonation
                   END-IF
                   GOBACK RETURNING ws-open-rc
               END-IF
           END-IF

           *> Find an empty slot in the connections table
           PERFORM VARYING ConnIX FROM 1 BY 1
                UNTIL ConnIX > MAX-CONNECTIONS
                OR ResMgrID(ConnIX) = 0
           END-PERFORM

           *> Initialize user personaliation and CICS enablement
           *> to default values, ie CICS/Web services enabled
           *> but no user personalization
           MOVE 1 TO TxEnabled(ConnIX)
           MOVE 1 TO LUsername(ConnIX)
           INITIALIZE DoImpersonate(ConnIX)
           MOVE 0 TO Impersonating(ConnIX)
           MOVE 0 TO EsLoggingLevel(ConnIX)
           SET SqlIsoDefault(ConnIX) to TRUE

           IF CustomizationExitRoutine not = null
               *> Get external configuration info
               PERFORM GetCustomizationParams
           END-IF

           PERFORM ParseOpenString

           *> If this is a TP SEP that isn't enabled for TP, 
           *> there is no more to do
           IF (XAi-SEP-type = sepIsCICS OR XAi-SEP-type = sepIsIMS
                       OR XAi-SEP-type = sepIsWebServices)
                   AND TxEnabled(ConnIX) = 0
               MOVE lk-rmid TO ResMgrID(ConnIX)
               GOBACK RETURNING XA-OK
           END-IF

           IF DsnString = LOW-VALUES
              MOVE -2 TO ws-open-rc

              IF 78-CTF-FLAG-LEVEL-ERROR >= ctf-trace-level
                  MOVE SPACES TO ls-trace-data-desc
                  STRING "ESODBCXA Open String Error(" 
                      XAi-XA-Resource-Name DELIMITED BY "  "
                      ")" DELIMITED BY SPACES
                      INTO ls-trace-data-desc
                  END-STRING
                  MOVE EVENT-ODBC-OPEN-BADOPENSTRING TO ls-trace-event
                  MOVE 78-CTF-FLAG-LEVEL-ERROR  TO ls-trace-level
                  PERFORM trace-odbc-event
              END-IF

           ELSE

           *> Connect to the database

              MOVE SPACES TO MFSQLMESSAGETEXT

              *> Connection name defaults to XA resource name
              MOVE XAi-XA-Resource-Name TO ConnectionName(ConnIX)

              MOVE ConnectionName(ConnIX) TO ThisConnName

           *> impersonation for Oracle when using MBDT
           *> only do this for Oracle by verifying USERP=O and BATCHONLY=T
           *> also make sure USRPASS has been set 
              IF XAi-SEP-type = sepIsJCL and doORACLE(ConnIX) = 1
                 and doBATCH(ConnIX) <> 1
                 and doCICS(ConnIX)  <> 1
                 and doIMS(ConnIX)   <> 1
                 and TxEnabled(ConnIX) = 0 
                 and UsrPassString <> LOW-VALUES

                  PERFORM GetImpersonatedUser
                  IF ImpersonatedUser NOT = SPACES
                      UNSTRING UsrPassString delimited by "."
                          INTO ProxyUserString, ProxyPassString 
                      move 1 to NewLen
                      move SPACES to NewUsrPassString
                      STRING 
                         ProxyUserString     DELIMITED SPACE
                         "["                 DELIMITED SIZE
                         ImpersonatedUser    DELIMITED SPACE
                         "]."                DELIMITED SIZE
                         ProxyPassString     DELIMITED SPACE
                      INTO NewUsrPassString pointer NewLen

      *               MOVE 1 TO MsgLen
      *               MOVE 1 TO MsgLevel
      *               STRING
      *                   "Old UsrPass " DELIMITED SIZE
      *                    UsrPassString      DELIMITED SIZE
      *               INTO consoleMessage pointer MsgLen
      *               SUBTRACT 1 FROM MsgLen
      *               CALL "mfxaLogConsoleMsg" USING
      *                          consoleMessage MsgLen
      *                          EsLoggingLevel(ConnIX) MsgLevel
      *               END-CALL
      *
      *               MOVE 1 TO MsgLen
      *               MOVE 1 TO MsgLevel
      *               STRING
      *                   "New UsrPass " DELIMITED SIZE
      *                    NewUsrPassString DELIMITED SIZE
      *               INTO consoleMessage pointer MsgLen
      *               SUBTRACT 1 FROM MsgLen
      *               CALL "mfxaLogConsoleMsg" USING
      *                          consoleMessage MsgLen
      *                          EsLoggingLevel(ConnIX) MsgLevel
      *               END-CALL
                      
                      EXEC SQL
                         CONNECT TO :DsnString  AS :ThisConnName
                         USER :NewUsrPassString
                      END-EXEC
                      
                      IF 78-CTF-FLAG-LEVEL-INFO >= ctf-trace-level
                          MOVE SPACES TO ls-trace-data-desc
                          STRING "ESODBCXA Execute as " ImpersonatedUser
                             DELIMITED BY SPACE
                          INTO ls-trace-data-desc
                          END-STRING
                          MOVE EVENT-ODBC-EXECUTEAS   TO ls-trace-event
                          MOVE 78-CTF-FLAG-LEVEL-INFO TO ls-trace-level
                          PERFORM trace-odbc-event
                      END-IF
                      
                      IF SQLCODE < 0
                          INSPECT MFSQLMESSAGETEXT REPLACING 
                              TRAILING SPACE BY X"FF"
                          MOVE 1 TO MsgLen
                          MOVE 1 TO MsgLevel
                          STRING
                              XAi-XA-Resource-Name DELIMITED SPACE
                              ": " DELIMITED SIZE
                              "User Impersonated as [" DELIMITED SIZE
                              ImpersonatedUser DELIMITED SPACE
                              "] failed." DELIMITED SIZE
                              MFSQLMESSAGETEXT DELIMITED X"FF"
                          INTO consoleMessage pointer MsgLen
                          SUBTRACT 1 FROM MsgLen
                          CALL "mfxaLogConsoleMsg" USING
                                     consoleMessage MsgLen
                                     EsLoggingLevel(ConnIX) MsgLevel
                          END-CALL
                          MOVE 0 TO Impersonating(ConnIX)
                          MOVE SPACES TO MFSQLMESSAGETEXT
                      ELSE
                          MOVE 1 TO Impersonating(ConnIX)
                      END-IF

                      INITIALIZE NewUsrPassString
                                 ProxyUserString
                                 ProxyPassString
                                 ImpersonatedUser
                  END-IF
              END-IF

              IF UsrPassString = LOW-VALUES
                    EXEC SQL
                       CONNECT TO :DsnString AS :ThisConnName
                    END-EXEC
              ELSE
                  IF Impersonating(ConnIX) = 0
                    EXEC SQL
                       CONNECT TO :DsnString  AS :ThisConnName
                       USER :UsrPassString
                    END-EXEC
                  ELSE
                    MOVE 0 TO Impersonating(ConnIX)
                  END-IF
              END-IF
   
              *> Discard credentials as soon as they are no longer needed
              INITIALIZE UsrPassString
              initialize password

      $IF XA-RECONNECT DEFINED
              *> Keep Connection string to enable database reconnection
              move lk-open-string to ConnectionString(ConnIX)
      $END

              IF SQLCODE NOT = ZERO
                 INSPECT MFSQLMESSAGETEXT REPLACING 
                    TRAILING SPACE BY X"FF"
                 MOVE 1 TO MsgLen
                 MOVE 1 TO MsgLevel
                 STRING
                    XAi-XA-Resource-Name DELIMITED SPACE
                    ": " DELIMITED SIZE
                    "Connect failed " 
                            DELIMITED SIZE
                    MFSQLMESSAGETEXT DELIMITED X"FF"
                    INTO consoleMessage POINTER MsgLen
                 END-STRING
                 SUBTRACT 1 FROM MsgLen
                 IF XAi-SEP-type = sepIsJCL
                   CALL "mfxaLogConsoleMsg" USING 
                      consoleMessage MsgLen 
                      EsLoggingLevel(ConnIX) MsgLevel  
                   END-CALL
                 ELSE
                 CALL "mfxaPutConsoleMsg" USING 
                    consoleMessage MsgLen  
                 END-CALL
                 END-IF
                 MOVE -3 TO ws-open-rc
              ELSE
                 MOVE 0  TO ws-open-rc

                 EXEC SQL
                     GET HDBC INTO :ThisConnHandle
                 END-EXEC

                 MOVE ThisConnHandle TO ConnectionHandle(ConnIX)

                 CALL odbcapi "SQLGetInfo" USING BY VALUE ThisConnHandle,
                                                 BY VALUE SqlDriverVer SIZE 2,
                                                 BY REFERENCE DriverVersion(ConnIx),
                                                 BY VALUE 11 SIZE 2,
                                                 BY REFERENCE SqlRtnSize
                                           RETURNING SqlRtn
                 END-CALL

      $if MFDBFH-SUPPORT defined
                 PERFORM mfdbfh-connection-register
      $end MFDBFH-SUPPORT defined

                 *> SQL COMMIT to ensure any SQL works after xa-open
                 *> and before xa-start is cleared.
                 EXEC SQL
                     COMMIT
                 END-EXEC
              END-IF

              IF 78-CTF-FLAG-LEVEL-INFO >= ctf-trace-level
                  MOVE SPACES TO ls-trace-data-desc
                  STRING "ESODBCXA Connect(" 
                      XAi-XA-Resource-Name DELIMITED BY "  "
                      ")" DELIMITED BY SPACES
                      INTO ls-trace-data-desc
                  END-STRING
                  MOVE EVENT-ODBC-OPEN-CONNECT     TO ls-trace-event
                  MOVE 78-CTF-FLAG-LEVEL-INFO TO ls-trace-level
                  PERFORM trace-odbc-event
              END-IF

              IF SQLCODE < 0
                 GOBACK RETURNING ws-open-rc
              END-IF

              *> Connection successful, reserve slot in connection table
              ADD 1 TO connectionCount
              MOVE lk-rmid        TO ResMgrID(ConnIX)
              MOVE XAi-XA-Resource-Name TO ResName(ConnIX)

              CALL "CBL_TOUPPER" USING ResName(ConnIX)
                               BY VALUE FUNCTION LENGTH(ResName(ConnIX))
              END-CALL

              *>Get the connection type
              EXEC SQL SET :TmpString = CURRENT DATABASE END-EXEC

              IF SQLCODE = 0
                 MOVE TmpString TO DatabaseType(ConnIX)
              ELSE
                 MOVE "OTHER" TO DatabaseType(ConnIX)
              END-IF

              *> FIXME - CTF trace

      $IF DYNAMICREGISTRATION = "Y" 
              *> Set the XA trigger for dynamic registration
              *> This will fire just once, when the application
              *> executes an EXEC SQL statement
              SET hvppTrigger TO ENTRY esxa-register

              EXEC SQL
                  SET XA TRIGGER :hvXaTrigger
              END-EXEC

              IF 78-CTF-FLAG-LEVEL-INFO >= ctf-trace-level
                  MOVE SPACES TO ls-trace-data-desc
                  STRING "ESODBCXA SET XA Trigger(" 
                      XAi-XA-Resource-Name DELIMITED BY "  "
                      ")" DELIMITED BY SPACES
                      INTO ls-trace-data-desc
                  END-STRING
                  MOVE EVENT-ODBC-XATRIGGER     TO ls-trace-event
                  MOVE 78-CTF-FLAG-LEVEL-INFO TO ls-trace-level
                  PERFORM trace-odbc-event
              END-IF

      $END

           END-IF

           *> Make sure to call UserImpersonation here if in batch mode
           IF XAi-SEP-type = sepIsJCL
              perform BeginUserImpersonation
           END-IF
       
           GOBACK RETURNING ws-open-rc
           .

       xa-2pc-close SECTION.
       ENTRY esxa-2pc-close USING BY REFERENCE lk-close-string
                                  BY VALUE     lk-rmid
                                  BY VALUE     lk-flags.

           MOVE SPACES TO MFSQLMESSAGETEXT

           *> Disconnect from the data source.
           MOVE lk-rmid TO ws-rmid

           *> Find the current ConnInfo based on RMID
           PERFORM VARYING ConnIX FROM 1 BY 1
                UNTIL ConnIX > MAX-CONNECTIONS
                IF ResMgrID(ConnIX) = lk-rmid AND
                   TxEnabled(ConnIX) = 0
                    *> BATCHONLY=T, skip all 2pc tasks and
                    *> Mark connection table entry free
                    MOVE 0 TO ResMgrID(ConnIX)
                    GOBACK RETURNING XA-OK
                END-IF
           END-PERFORM

           PERFORM SetCurrentConnection
           MOVE "CLOSE" TO LastCall(ConnIX)

           call esxa-close using
                by reference lk-close-string
                by value     lk-rmid
                by value     lk-flags
                returning    ws-close-rc
           end-call
		   
           GOBACK RETURNING ws-close-rc
           .
		   
       xa-close SECTION.
       ENTRY esxa-close USING BY REFERENCE lk-close-string
                                  BY VALUE     lk-rmid
                                  BY VALUE     lk-flags.

           MOVE SPACES TO MFSQLMESSAGETEXT

           *> Disconnect from the data source.
           MOVE lk-rmid TO ws-rmid

           PERFORM SetCurrentConnection
           MOVE "CLOSE" TO LastCall(ConnIX)

      $if MFDBFH-SUPPORT defined
           PERFORM mfdbfh-connection-deregister
      $end MFDBFH-SUPPORT defined

           *> If this is a TP SEP that isn't enabled for TP, 
           *> there is no more to do
           IF (XAi-SEP-type = sepIsCICS OR XAi-SEP-type = sepIsIMS
                       OR XAi-SEP-type = sepIsWebServices)
                   AND TxEnabled(ConnIX) = 0
               *> Mark connection table entry free
               MOVE 0 TO ResMgrID(ConnIX)
               GOBACK RETURNING XA-OK
           END-IF


      $IF DYNAMICREGISTRATION = "Y" 
           MOVE 0 TO hvXaTrigger
           EXEC SQL
               SET XA TRIGGER :hvXaTrigger
           END-EXEC

           IF 78-CTF-FLAG-LEVEL-INFO >= ctf-trace-level
                  MOVE SPACES TO ls-trace-data-desc
                  STRING "ESODBCXA SET XA Trigger(" 
                      XAi-XA-Resource-Name DELIMITED BY "  "
                      ")" DELIMITED BY SPACES
                      INTO ls-trace-data-desc
                  END-STRING
               MOVE EVENT-ODBC-XATRIGGER     TO ls-trace-event
               MOVE 78-CTF-FLAG-LEVEL-INFO TO ls-trace-level
               PERFORM trace-odbc-event
           END-IF

      $END

              EXEC SQL
                   DISCONNECT CURRENT
              END-EXEC

           IF SQLCODE NOT = ZERO
              MOVE -3 TO ws-close-rc
           ELSE
              MOVE 0  TO ws-close-rc
           END-IF

           MOVE SPACES TO ResName(ConnIX) ConnectionName(ConnIX)
           MOVE 0 TO ResMgrID(ConnIX)

           IF 78-CTF-FLAG-LEVEL-INFO >= ctf-trace-level
               MOVE SPACES TO ls-trace-data-desc
               STRING "ESODBCXA Disconnect(" 
                   ResName(ConnIX) DELIMITED BY "  "
                   ")" DELIMITED BY SPACES
                   INTO ls-trace-data-desc
               END-STRING
               MOVE EVENT-ODBC-CLOSE            TO ls-trace-event
               MOVE 78-CTF-FLAG-LEVEL-INFO TO ls-trace-level
               PERFORM trace-odbc-event
           END-IF

           GOBACK RETURNING ws-close-rc
           .

      $IF DYNAMICREGISTRATION = "Y" 
       *> This is a callback made by the OpenESQL runtime
       *> when a previosuly set dynamic registration trigger fires
       xa-register SECTION.
       ENTRY esxa-register cdecl USING BY REFERENCE lk-xaid.

           MOVE lk-xaid TO ws-xaid
           INSPECT ws-xaid REPLACING TRAILING LOW-VALUES BY SPACE
           CALL "CBL_TOUPPER" USING ws-xaid
                              BY VALUE FUNCTION LENGTH(ws-xaid)
           END-CALL
           
           *> Find ConnInfo for this resources
           IF ws-xaid = '*' OR ws-xaid = SPACES
               *> TODO - verify there is only 1 XA resource defined and log an error?
               MOVE 1 TO ConnIX
           ELSE
               PERFORM VARYING ConnIX FROM 1 BY 1 
                       UNTIL ConnIX > MAX-CONNECTIONS 
                   IF ResName(ConnIX) = ws-xaid 
                           AND ResMgrID(ConnIX) NOT = 0
                       EXIT PERFORM
                   END-IF
               END-PERFORM
           END-IF
           
           IF ConnIX > MAX-CONNECTIONS
               *> TODO - log an error?
               MOVE 1 TO ConnIX          
           END-IF
           
           MOVE ResMgrID(ConnIX) TO ws-rmid                         
           CALL "ax_reg" USING BY VALUE ws-rmid,
                               BY REFERENCE XID,
                               BY VALUE TMNOFLAGS
                         RETURNING ws-register-rc
           END-CALL
    
           MOVE ws-register-rc       TO ctftrace-rc

           IF 78-CTF-FLAG-LEVEL-INFO >= ctf-trace-level
               MOVE SPACES TO ls-trace-data-desc
               STRING "ESODBCXA xa_register(" 
                   XAi-XA-Resource-Name DELIMITED BY "  "
                   ")" DELIMITED BY SPACES
                   INTO ls-trace-data-desc
               END-STRING
               MOVE EVENT-MSSQL-AXREG      TO ls-trace-event
               MOVE 78-CTF-FLAG-LEVEL-INFO TO ls-trace-level
               PERFORM trace-odbc-event
           END-IF

           EVALUATE ws-register-rc
           WHEN TM-JOIN
           WHEN TM-RESUME
           WHEN TM-OK

               CALL esxa-start USING BY REFERENCE XID
                                  BY VALUE     ResMgrID(ConnIX)
                                  BY VALUE     TMNOFLAGS
                         RETURNING ws-start-rc
               END-CALL

               MOVE ws-start-rc       TO ctftrace-rc

               IF 78-CTF-FLAG-LEVEL-INFO >= ctf-trace-level
                   MOVE SPACES TO ls-trace-data-desc
                   STRING "ESODBCXA xa_start(" 
                       XAi-XA-Resource-Name DELIMITED BY "  "
                       ")" DELIMITED BY SPACES
                       INTO ls-trace-data-desc
                   END-STRING
                   MOVE EVENT-MSSQL-XASTART    TO ls-trace-event
                   MOVE 78-CTF-FLAG-LEVEL-INFO TO ls-trace-level
                   PERFORM trace-odbc-event
               END-IF

           WHEN OTHER
               *> now what?
               *> Would it be OK to just reset the trigger
               *> and hope for the best
               CONTINUE
           END-EVALUATE
                  
           GOBACK.
      $end
       
       xa-start SECTION.
       ENTRY esxa-start USING BY REFERENCE lk-xid
                                  BY VALUE     lk-rmid
                                  BY VALUE     lk-flags.

           MOVE SPACES TO MFSQLMESSAGETEXT

           MOVE lk-rmid TO ws-rmid

           *> Find the current ConnInfo based on RMID
           PERFORM VARYING ConnIX FROM 1 BY 1
             UNTIL ConnIX > MAX-CONNECTIONS
               IF ResMgrID(ConnIX) = lk-rmid AND
                 TxEnabled(ConnIX) = 0
                   *> BATCHONLY=T, skip all 2pc tasks.
                   GOBACK RETURNING XA-OK
               END-IF
           END-PERFORM

           PERFORM SetCurrentConnection
           MOVE "START" TO LastCall(ConnIX)

      $IF XA-RECONNECT DEFINED
           MOVE 0 TO ws-start-rc
           CALL esxa-ping USING BY VALUE lk-rmid
                                BY REFERENCE ls-swtype
           END-CALL
           IF ws-connect-fail-88

               *> There is an issue here with the database connection.
               *> We attempt to reconnect to the database to cater for a
               *> temp network glitch.

               *> Log the connection issue if ESLOGGING Enabled
               MOVE 1 TO MsgLen
               MOVE 1 TO MsgLevel
               STRING
                   XAi-XA-Resource-Name DELIMITED SPACE
                   ": " DELIMITED SIZE
                   "XA Resource connection lost." 
                           DELIMITED SIZE
               INTO consoleMessage POINTER MsgLen
               END-STRING
               SUBTRACT 1 FROM MsgLen
               CALL "mfxaLogConsoleMsg" USING 
                  consoleMessage MsgLen 
                  EsLoggingLevel(ConnIX) MsgLevel  
               END-CALL
               
               call esxa-close using
                    by reference " "
                    by value     lk-rmid
                    by value     lk-flags
                    returning    ws-close-rc
               end-call

               perform ws-conn-retry times
                 call esxa-open using
                      by reference ConnectionString(ConnIX)
                      by value     lk-rmid
                      by value     lk-flags
                      returning    ws-open-rc
                 end-call
                 if ws-open-rc = xa-OK

                     *> Reconnect successfully
                     MOVE 1 TO MsgLen
                     MOVE 1 TO MsgLevel
                     STRING
                        XAi-XA-Resource-Name DELIMITED SPACE
                        ": " DELIMITED SIZE
                        "XA Resource reconnects successfully." 
                            DELIMITED SIZE
                     INTO consoleMessage POINTER MsgLen
                     END-STRING
                     SUBTRACT 1 FROM MsgLen
                     CALL "mfxaLogConsoleMsg" USING 
                         consoleMessage MsgLen 
                         EsLoggingLevel(ConnIX) MsgLevel  
                     END-CALL
          
                     exit perform
                 end-if

                 *> Reconnect successfully
                 MOVE 1 TO MsgLen
                 MOVE 1 TO MsgLevel
                 STRING
                    XAi-XA-Resource-Name DELIMITED SPACE
                    ": " DELIMITED SIZE
                    "XA Resource failed to reconnect." 
                        DELIMITED SIZE
                 INTO consoleMessage POINTER MsgLen
                 END-STRING
                 SUBTRACT 1 FROM MsgLen
                 CALL "mfxaLogConsoleMsg" USING 
                     consoleMessage MsgLen 
                     EsLoggingLevel(ConnIX) MsgLevel  
                 END-CALL

                 call "CBL_THREAD_SLEEP"
                      using by value ws-sleep-time
                 end-call
              end-perform
              move ws-open-rc to ws-start-rc
           END-IF

           IF ws-start-rc NOT = XA-OK
              GOBACK RETURNING ws-start-rc
           END-IF
   
           PERFORM SetCurrentConnection
      $END

           PERFORM BeginUserImpersonation

           EXEC SQL
                BEGIN TRANSACTION
           END-EXEC

           IF SQLCODE NOT = ZERO
              MOVE -3 TO ws-start-rc
           ELSE
              MOVE 0  TO ws-start-rc
           END-IF
           
           IF ws-start-rc = XA-OK AND NOT SqlIsoDefault(ConnIX)
              *> Set Transaction's isolation level
              PERFORM SetTransIsolationLevel
           END-IF

           IF ws-start-rc = XA-OK 
               CALL esxa-ping USING BY VALUE lk-rmid
                                    BY REFERENCE ls-swtype
               END-CALL
               IF ws-connect-fail-88
                   MOVE XAER-RMERR TO ws-start-rc
               END-IF
           END-IF

           IF 78-CTF-FLAG-LEVEL-INFO >= ctf-trace-level
               MOVE SPACES TO ls-trace-data-desc
               STRING "ESODBCXA Begin Transaction(" 
                   XAi-XA-Resource-Name DELIMITED BY "  "
                   ")" DELIMITED BY SPACES
                   INTO ls-trace-data-desc
               END-STRING
               MOVE EVENT-ODBC-START            TO ls-trace-event
               MOVE 78-CTF-FLAG-LEVEL-INFO TO ls-trace-level
               PERFORM trace-odbc-event
               *>PERFORM trc-xid
           END-IF

           GOBACK RETURNING ws-start-rc
           .

       xa-end SECTION.
       ENTRY esxa-end USING BY REFERENCE lk-xid
                                BY VALUE     lk-rmid
                                BY VALUE     lk-flags.

           MOVE lk-rmid TO ws-rmid

           *> Find the current ConnInfo based on RMID
           PERFORM VARYING ConnIX FROM 1 BY 1
             UNTIL ConnIX > MAX-CONNECTIONS
               IF ResMgrID(ConnIX) = lk-rmid AND
                 TxEnabled(ConnIX) = 0
                   *> BATCHONLY=T, skip all 2pc tasks.
                   GOBACK RETURNING XA-OK
               END-IF
           END-PERFORM

           PERFORM SetCurrentConnection
           MOVE "END" TO lastCall(ConnIX)

           MOVE 0 TO ws-end-rc

      $IF DYNAMICREGISTRATION = "Y"
           *> Reset the XA trigger in case the application
           *> didn't execute any EXEC SQL statemenst
           move 0 to hvXaTrigger
           EXEC SQL
               SET XA TRIGGER :hvXaTrigger
           END-EXEC
            
           IF 78-CTF-FLAG-LEVEL-INFO >= ctf-trace-level
               MOVE SPACES TO ls-trace-data-desc
               STRING "ESODBCXA SET XA Trigger(" 
                   XAi-XA-Resource-Name DELIMITED BY "  "
                   ")" DELIMITED BY SPACES
                   INTO ls-trace-data-desc
               END-STRING
               MOVE EVENT-ODBC-XATRIGGER     TO ls-trace-event
               MOVE 78-CTF-FLAG-LEVEL-INFO TO ls-trace-level
               PERFORM trace-odbc-event
           END-IF

      $END

           PERFORM RestoreApplicationConnection 
           GOBACK RETURNING ws-end-rc
           .

       xa-prepare SECTION.
       ENTRY esxa-prepare USING BY REFERENCE lk-xid
                                    BY VALUE     lk-rmid
                                    BY VALUE     lk-flags.

           MOVE 0 TO ws-prepare-rc

           MOVE lk-rmid TO ws-rmid

           *> Find the current ConnInfo based on RMID
           PERFORM VARYING ConnIX FROM 1 BY 1
             UNTIL ConnIX > MAX-CONNECTIONS
               IF ResMgrID(ConnIX) = lk-rmid AND
                 TxEnabled(ConnIX) = 0
                   *> BATCHONLY=T, skip all 2pc tasks.
                   GOBACK RETURNING XA-OK
               END-IF
           END-PERFORM

           PERFORM SetCurrentConnection
           MOVE "PREPARE" TO LastCall(ConnIX)
         
           PERFORM RestoreApplicationConnection            
           GOBACK RETURNING ws-prepare-rc
           .

       xa-2pc-commit SECTION.
       ENTRY esxa-2pc-commit  USING BY REFERENCE lk-xid
                                    BY VALUE     lk-rmid
                                    BY VALUE     lk-flags.

           MOVE SPACES TO MFSQLMESSAGETEXT

           *> Commit the transaction

           MOVE lk-rmid TO ws-rmid

           *> Find the current ConnInfo based on RMID
           PERFORM VARYING ConnIX FROM 1 BY 1
             UNTIL ConnIX > MAX-CONNECTIONS
               IF ResMgrID(ConnIX) = lk-rmid AND
                 TxEnabled(ConnIX) = 0
                   *> BATCHONLY=T, skip all 2pc tasks.
                   GOBACK RETURNING XA-OK
               END-IF
           END-PERFORM

           PERFORM SetCurrentConnection
           MOVE "COMMIT" to LastCall(ConnIX)
		   
           call esxa-commit using
                by reference lk-xid
                by value     lk-rmid
                by value     lk-flags
                returning    ws-commit-rc
           end-call 
            
           GOBACK RETURNING ws-commit-rc
           .


       xa-commit SECTION.
       ENTRY esxa-commit  USING BY REFERENCE lk-xid
                                    BY VALUE     lk-rmid
                                    BY VALUE     lk-flags.

           MOVE SPACES TO MFSQLMESSAGETEXT

           *> Commit the transaction

           MOVE lk-rmid TO ws-rmid
           PERFORM SetCurrentConnection
           MOVE "COMMIT" to LastCall(ConnIX)
            
           *> If this is a TP SEP that isn't enabled for TP, 
           *> there is no more to do
           IF (XAi-SEP-type = sepIsCICS OR XAi-SEP-type = sepIsIMS
                       OR XAi-SEP-type = sepIsWebServices)
                   AND TxEnabled(ConnIX) = 0
               GOBACK RETURNING XA-OK
           END-IF
            
           IF (XAi-SEP-type = sepIsCICS OR
               XAi-SEP-type = sepIsWebServices)
               CALL "mfxaGetEotFlag" USING
                  BY REFERENCE ws-boolCICSEOT
               END-CALL
           ELSE
               MOVE 0 TO ws-boolCICSEOT
           END-IF

           IF ws-boolCICSEOT = 0
               *> This is EXEC CICS SYNCPOINT COMMIT in an application
               *> Ensure that any open cursors not defined as WITH HOLD
               *> get closed and HCOSS global temp tables are processed as appropriate
               *> Do this outside the global transaction in autocommit mode
               *> There are timing/locking/logic difficulties otherwise

                EXEC SQL
                    SYNCPOINT COMMIT
                END-EXEC

                IF 78-CTF-FLAG-LEVEL-INFO >= ctf-trace-level
                    MOVE SPACES TO ls-trace-data-desc
                    STRING "ESODBCXA Syncpoint Commit(" 
                        XAi-XA-Resource-Name DELIMITED BY "  "
                        ")" DELIMITED BY SPACES
                        INTO ls-trace-data-desc
                    END-STRING
                    MOVE EVENT-ODBC-SYNCPOINT        TO ls-trace-event
                    MOVE 78-CTF-FLAG-LEVEL-INFO TO ls-trace-level
                    PERFORM trace-odbc-event
                END-IF
           EXEC SQL
                COMMIT TRANSACTION
           END-EXEC

           IF SQLCODE NOT = ZERO
              MOVE -3 TO ws-commit-rc
           ELSE
              MOVE 0  TO ws-commit-rc
           END-IF

           IF 78-CTF-FLAG-LEVEL-INFO >= ctf-trace-level
               MOVE SPACES TO ls-trace-data-desc
               STRING "ESODBCXA Commit(" 
                   ResName(ConnIX) DELIMITED BY "  "
                   ")" DELIMITED BY SPACES
                   INTO ls-trace-data-desc
               END-STRING
               MOVE EVENT-ODBC-COMMIT           TO ls-trace-event
               MOVE 78-CTF-FLAG-LEVEL-INFO TO ls-trace-level
               PERFORM trace-odbc-event
               *>PERFORM trc-xid
           END-IF

           PERFORM EndUserImpersonation


           ELSE
               *> CICS EOT, so more complete clean up
               *> Close all cursors and drop all HCOSS global temp tables
               *> Do this in autocommit mode, outside of a transaction
               *> after we have de-enlisted
           EXEC SQL
                COMMIT TRANSACTION
           END-EXEC

           IF SQLCODE NOT = ZERO
              MOVE -3 TO ws-commit-rc
           ELSE
              MOVE 0  TO ws-commit-rc
           END-IF

           IF 78-CTF-FLAG-LEVEL-INFO >= ctf-trace-level
               MOVE SPACES TO ls-trace-data-desc
               STRING "ESODBCXA Commit(" 
                   ResName(ConnIX) DELIMITED BY "  "
                   ")" DELIMITED BY SPACES
                   INTO ls-trace-data-desc
               END-STRING
               MOVE EVENT-ODBC-COMMIT           TO ls-trace-event
               MOVE 78-CTF-FLAG-LEVEL-INFO TO ls-trace-level
               PERFORM trace-odbc-event
               *>PERFORM trc-xid
           END-IF

           PERFORM EndUserImpersonation


                EXEC SQL
                    RESET CONNECTION
                END-EXEC

                IF 78-CTF-FLAG-LEVEL-INFO >= ctf-trace-level
                    MOVE SPACES TO ls-trace-data-desc
                    STRING "ESODBCXA Reset Connection(" 
                        XAi-XA-Resource-Name DELIMITED BY "  "
                        ")" DELIMITED BY SPACES
                        INTO ls-trace-data-desc
                    END-STRING
                    MOVE EVENT-ODBC-RESETCONN        TO ls-trace-event
                    MOVE 78-CTF-FLAG-LEVEL-INFO TO ls-trace-level
                    PERFORM trace-odbc-event
                END-IF
           END-IF
           
      *     EXEC SQL
      *          COMMIT TRANSACTION
      *     END-EXEC

      $IF DYNAMICREGISTRATION = "Y"
           SET hvppTrigger TO ENTRY esxa-register

           EXEC SQL
               SET XA TRIGGER :hvXaTrigger
           END-EXEC

           IF 78-CTF-FLAG-LEVEL-INFO >= ctf-trace-level
               MOVE SPACES TO ls-trace-data-desc
               STRING "ESODBCXA SET XA Trigger(" 
                   XAi-XA-Resource-Name DELIMITED BY "  "
                   ")" DELIMITED BY SPACES
                   INTO ls-trace-data-desc
               END-STRING
               MOVE EVENT-ODBC-XATRIGGER     TO ls-trace-event
               MOVE 78-CTF-FLAG-LEVEL-INFO TO ls-trace-level
               PERFORM trace-odbc-event
           END-IF

      $if MFDBFH-SUPPORT defined
           perform mfdbfh-reset-xa-trigger
      $end MFDBFH-SUPPORT defined
      $END

           PERFORM RestoreApplicationConnection 
           GOBACK RETURNING ws-commit-rc
           .

       xa-2pc-rollback SECTION.
       ENTRY esxa-2pc-rollback  USING BY REFERENCE lk-xid
                                      BY VALUE     lk-rmid
                                      BY VALUE     lk-flags.

           MOVE SPACES TO MFSQLMESSAGETEXT

           *> Rollback the transaction

           MOVE lk-rmid TO ws-rmid

           *> Find the current ConnInfo based on RMID
           PERFORM VARYING ConnIX FROM 1 BY 1
             UNTIL ConnIX > MAX-CONNECTIONS
               IF ResMgrID(ConnIX) = lk-rmid AND
                 TxEnabled(ConnIX) = 0
                   *> BATCHONLY=T, skip all 2pc tasks.
                   GOBACK RETURNING XA-OK
               END-IF
           END-PERFORM

           PERFORM SetCurrentConnection
		   
           call esxa-rollback using
                by reference lk-xid
                by value     lk-rmid
                by value     lk-flags
                returning    ws-rollback-rc
           end-call 
		   
           GOBACK RETURNING ws-rollback-rc
           .

       xa-rollback SECTION.
       ENTRY esxa-rollback  USING BY REFERENCE lk-xid
                                      BY VALUE     lk-rmid
                                      BY VALUE     lk-flags.

           MOVE SPACES TO MFSQLMESSAGETEXT

           *> Rollback the transaction

           MOVE lk-rmid TO ws-rmid
           PERFORM SetCurrentConnection
           
           *> If this is a TP SEP that isn't enabled for TP, 
           *> there is no more to do
           IF (XAi-SEP-type = sepIsCICS OR XAi-SEP-type = sepIsIMS
                       OR XAi-SEP-type = sepIsWebServices)
                   AND TxEnabled(ConnIX) = 0
               GOBACK RETURNING XA-OK
           END-IF
           
           IF LastCall(ConnIx) NOT = 'PREPARE'

               *> ES will skip the prepare call if the rollback
               *> came via EXEC CICS SYNCPOINT ROLLBACK
               *> in this case it doesn't need the prepare, it
               *> it just goes stright to rollback for each RM

               EXEC SQL
                    SYNCPOINT ROLLBACK
               END-EXEC

               IF 78-CTF-FLAG-LEVEL-INFO >= ctf-trace-level
                   MOVE SPACES TO ls-trace-data-desc
                   STRING "ESODBCXA Syncpoint Rollback(" 
                       ResName(ConnIX) DELIMITED BY "  "
                       ")" DELIMITED BY SPACES
                       INTO ls-trace-data-desc
                   END-STRING
                   MOVE EVENT-ODBC-SYNCROLLBK     TO ls-trace-event
                   MOVE 78-CTF-FLAG-LEVEL-INFO TO ls-trace-level
                   PERFORM trace-odbc-event
               END-IF

           END-IF
           
           MOVE "ROLLBACK" TO LastCall(ConnIX)
           
           EXEC SQL
                ROLLBACK TRANSACTION
           END-EXEC

           IF SQLCODE NOT = ZERO
              MOVE -3 TO ws-rollback-rc
              IF SQLSTATE = "08S01" OR
                 SQLSTATE = "08S02"
                 Call "mF_SetContDirty"
              END-IF
           ELSE
              MOVE 0  TO ws-rollback-rc
           END-IF

           IF 78-CTF-FLAG-LEVEL-INFO >= ctf-trace-level
               MOVE SPACES TO ls-trace-data-desc
               STRING "ESODBCXA Rollback(" 
                   XAi-XA-Resource-Name DELIMITED BY "  "
                   ")" DELIMITED BY SPACES
                   INTO ls-trace-data-desc
               END-STRING
               MOVE EVENT-ODBC-ROLLBACK         TO ls-trace-event
               MOVE 78-CTF-FLAG-LEVEL-INFO TO ls-trace-level
               PERFORM trace-odbc-event
               *>PERFORM trc-xid
           END-IF

           PERFORM EndUserImpersonation

           IF (XAi-SEP-type = sepIsCICS OR
               XAi-SEP-type = sepIsWebServices)
               CALL "mfxaGetEotFlag" USING
                  BY REFERENCE ws-boolCICSEOT
               END-CALL
           ELSE
               MOVE 0 TO ws-boolCICSEOT
           END-IF

           IF ws-boolCICSEOT = 1
               *> CICS EOT, so more complete clean up
               *> Close all cursors and drop all HCOSS global temp tables
               *> Do this in autocommit mode, outside of a transaction

           EXEC SQL
                    RESET HOST CONNECTION
           END-EXEC

           EXEC SQL
                COMMIT TRANSACTION
           END-EXEC

           IF 78-CTF-FLAG-LEVEL-INFO >= ctf-trace-level
               MOVE SPACES TO ls-trace-data-desc
               STRING "ESODBCXA Reset Host Connection(" 
                   XAi-XA-Resource-Name DELIMITED BY "  "
                   ")" DELIMITED BY SPACES
                   INTO ls-trace-data-desc
               END-STRING
               MOVE EVENT-ODBC-RSETHSTCON  TO ls-trace-event
               MOVE 78-CTF-FLAG-LEVEL-INFO TO ls-trace-level
               PERFORM trace-odbc-event
           END-IF

           END-IF

      $IF DYNAMICREGISTRATION = "Y"
           SET hvppTrigger TO ENTRY esxa-register

           EXEC SQL
               SET XA TRIGGER :hvXaTrigger
           END-EXEC

           IF 78-CTF-FLAG-LEVEL-INFO >= ctf-trace-level
               MOVE SPACES TO ls-trace-data-desc
               STRING "ESODBCXA SET XA Trigger(" 
                   XAi-XA-Resource-Name DELIMITED BY "  "
                   ")" DELIMITED BY SPACES
                   INTO ls-trace-data-desc
               END-STRING
               MOVE EVENT-ODBC-XATRIGGER     TO ls-trace-event
               MOVE 78-CTF-FLAG-LEVEL-INFO TO ls-trace-level
               PERFORM trace-odbc-event
           END-IF

      $if MFDBFH-SUPPORT defined
           perform mfdbfh-reset-xa-trigger
      $end MFDBFH-SUPPORT defined
      $END

           PERFORM RestoreApplicationConnection 
           GOBACK RETURNING ws-rollback-rc
           .

       xa-recover SECTION.
       ENTRY esxa-recover USING BY REFERENCE lk-xid
                                    BY VALUE     lk-count
                                    BY VALUE     lk-rmid
                                    BY VALUE     lk-flags.

           *> xa_recover is not supported at this time.

           MOVE -99 TO ws-recover-rc

           GOBACK RETURNING ws-recover-rc
           .

       xa-forget SECTION.
       ENTRY esxa-forget USING BY REFERENCE lk-xid
                                   BY VALUE     lk-rmid
                                   BY VALUE     lk-flags.

           MOVE 0 TO ws-forget-rc

           GOBACK RETURNING ws-forget-rc
           .

       xa-complete SECTION.
       ENTRY esxa-complete USING BY REFERENCE lk-xid
                                     BY VALUE     lk-rmid
                                     BY VALUE     lk-flags.

           MOVE 0 TO ws-complete-rc

           GOBACK RETURNING ws-complete-rc
           .

       xa-ping SECTION.
       ENTRY esxa-ping USING BY VALUE lk-rmid
                             BY REFERENCE lk-swtype.

              MOVE 5 to lk-swtype   
              MOVE lk-rmid TO ws-rmid
              PERFORM SetCurrentConnection
              PERFORM probeDatabase
           
              *> BATCHONLY=T, skip all 2pc tasks.
              IF TxEnabled(ConnIX) = 0
                 SET ws-connect-ok-88 TO TRUE
                 GOBACK RETURNING ws-connect-status
              END-IF

              EVALUATE DatabaseType(ConnIX)
              WHEN "SQLSERVER"
                 CALL odbcapi "SQLGetConnectAttr" USING
                            BY VALUE ThisConnHandle,
                            BY VALUE SqlCoptSSConnectionDead SIZE 4,
                            BY REFERENCE ws-connection-dead,
                            BY VALUE 4 SIZE 4,
                                    BY VALUE NullPtr
                          RETURNING SqlRtn
                  END-CALL
              WHEN "DB2"
                       *> Initialize Db2PingTime
                       MOVE 0 to Db2PingTime
                       CALL odbcapi "SQLGetConnectAttr" USING
                            BY VALUE ThisConnHandle,
                            BY VALUE SqlAttrPingDb SIZE 4,
                            BY REFERENCE Db2PingTime,
      $IF P64 SET
                            BY VALUE 8 SIZE 4,
      $ELSE
                            BY VALUE 4 SIZE 4,
      $END
                            BY VALUE NullPtr
                          RETURNING SqlRtn
                       END-CALL
              WHEN "POSTGRESQL"
                       MOVE MajorVer(ConnIx) TO curDriverVer
                       IF curDriverVer LESS THAN 12
                           IF SQLSTATE = '42P01'
                               SET ws-connect-ok-88 TO TRUE
                           ELSE
                               SET ws-connect-fail-88 TO TRUE
                           END-IF
                           GOBACK RETURNING ws-connect-status
                       ELSE
                           CALL odbcapi "SQLGetConnectAttr" USING
                                BY VALUE ThisConnHandle,
                                BY VALUE SqlAttrConnectionDead SIZE 4,
                                BY REFERENCE ws-connection-dead,
                                BY VALUE 4 SIZE 4,
                                BY VALUE NullPtr
                              RETURNING SqlRtn
                           END-CALL
                       END-IF
              WHEN "MYSQL"
                       IF SQLSTATE = 'S0002' AND SQLCODE = -1146
                           SET ws-connect-ok-88 TO TRUE
                       ELSE
                           SET ws-connect-fail-88 TO TRUE
                       END-IF
                       GOBACK RETURNING ws-connect-status
              WHEN "OTHER"
                       *> Unknown database (including XDB)
                       *> Always return connection OK
                       SET ws-connect-ok-88 TO TRUE
                       GOBACK RETURNING ws-connect-status
              WHEN OTHER
                       *> Oracle, MySQL
                       CALL odbcapi "SQLGetConnectAttr" USING
                            BY VALUE ThisConnHandle,
                            BY VALUE SqlAttrConnectionDead SIZE 4,
                            BY REFERENCE ws-connection-dead,
                            BY VALUE 4 SIZE 4,
                            BY VALUE NullPtr
                          RETURNING SqlRtn
                       END-CALL
              END-EVALUATE 

              EVALUATE SqlRtn
              WHEN -1  *> SQL_ERROR
              WHEN 2   *> SQL_STILL_EXECUTING
              WHEN -2 *> SQL_INVALID_HANDLE
                SET ws-connect-fail-88 TO TRUE
              WHEN 0  *> SQL_SUCCESS
              WHEN 1  *> SQL_SUCCESS_WITH_INFO
                 IF DatabaseType(ConnIX) = "DB2"
                    IF Db2PingTime = 0
                        SET ws-connect-fail-88 TO TRUE
                    ELSE
                        SET ws-connect-ok-88 TO TRUE
                    END-IF
                 ELSE
                     IF ws-connection-dead b-and 1 = SqlCdFalse
                       SET ws-connect-ok-88 TO TRUE
                     ELSE
                       SET ws-connect-fail-88 TO TRUE
                     END-IF
                 END-IF
              END-EVALUATE

           GOBACK RETURNING ws-connect-status.

      *---------------------------------------------------------
      * Helper routines
      *---------------------------------------------------------
       Customization section.
       
        *> CUSTOMIZE
        *> If you don't want to make the userID and password visible
        *> within the ES configuration pages or you want to override 
        *> the entire open place your customizations here. 
        *>
        *> To customize the User ID and Password, you should 
        *> populate ws-ext-usrpass in this routine and
        *> the USRPASS keyword in the open string shoould have the
        *> values %USRPASS%
        *>
        *> For example: 
        *>
        *> move "myUserId.myPassword" to ws-ext-usrpass
        *>
        *> To customize the entire open string populate ws-open-string
        *> in this routine
        *>
        *> For example: 
        *>
        *> MOVE 
        *>   z"DSN=myDbAlias,USRPASS=myUid.myPwd"
        *> TO lk-open-string
        *>
        *> Please refer to "SQL xa_open string Configuration Options"
        *> in Enterprise Server documentation for ES-specific options
        *> that may be configured in the XA open string 
        *>
           exit section.
       
       ParseOpenString SECTION.             
           IF CustomizationExitRoutine not = null
               PERFORM ExpandOpenStringMacros
               *>ws-open-STRING holds the open string after the open string parsing for the vault
               MOVE ws-open-STRING TO lk-open-string
               initialize ws-open-STRING
           ELSE
               PERFORM Customization
           END-IF

           MOVE 0 to parseErrorFlag
           MOVE 1 TO OuterPosn

           PERFORM UNTIL OuterPosn > 256

             *> From the start point, loop until the end of param
             *> or end of string
                PERFORM VARYING InnerPosn 
                    FROM OuterPosn BY 1 UNTIL
                       lk-open-string(InnerPosn : 1) = ","
                    OR lk-open-string(InnerPosn : 1) = LOW-VALUES
                END-PERFORM

             *> Remove any leading spaces
                PERFORM WITH TEST BEFORE UNTIL
                    lk-open-string(OuterPosn:1) NOT = SPACE
                    ADD 1 TO OuterPosn
                END-PERFORM

                MOVE lk-open-string(OuterPosn : InnerPosn - OuterPosn)
                     TO TmpWorkString

             *> Separate the Parameter and Value
                UNSTRING TmpWorkString DELIMITED BY "="
                         INTO ParamString, ValueString
                END-UNSTRING

                CALL "CBL_TOUPPER" USING ParamString
                                   BY VALUE 256
                END-CALL

                EVALUATE ParamString
                WHEN "DSN"
                     MOVE ValueString TO DsnString
                WHEN "USRPASS"
                       IF ValueString = "%USRPASS%"
                           MOVE ws-ext-usrpass TO UsrPassString
                       ELSE
                           MOVE ValueString TO UsrPassString
                       END-IF
               WHEN "USERP"
                   IF ValueString(1:1) = 'T' OR ValueString(1:1) = 't'
                       MOVE 1 TO DoBATCH(ConnIX)
                       MOVE 1 TO DoCICS(ConnIX)
                       MOVE 1 TO DoIMS(ConnIX)
                       MOVE 1 TO DoORACLE(ConnIX)
                   ELSE 
                   IF ValueString(1:1) = 'B' OR ValueString(1:1) = 'b'
                       MOVE 1 TO DoBATCH(ConnIX)
                   ELSE
                   IF ValueString(1:1) = 'C' OR ValueString(1:1) = 'c'
                       MOVE 1 TO DoCICS(ConnIX)
                   ELSE
                   IF ValueString(1:1) = 'I' OR ValueString(1:1) = 'i'
                       MOVE 1 TO DoIMS(ConnIX)
                   ELSE
                   IF ValueString(1:1) = 'O' OR ValueString(1:1) = 'o'
                       MOVE 1 TO DoORACLE(ConnIX)
                   ELSE
                       INITIALIZE DoImpersonate(ConnIX)
                   END-IF
                   END-IF
                   END-IF
                   END-IF
                   END-IF
               WHEN "BATCHONLY"       
                   IF ValueString(1:1) = 'T' OR ValueString(1:1) = 't'
                       MOVE 0 TO TxEnabled(ConnIX)
                   ELSE
                       MOVE 1 TO TxEnabled(ConnIX)
                   END-IF
               WHEN "LNAME"       
                   IF ValueString(1:1) = 'T' OR ValueString(1:1) = 't'
                       MOVE 0 TO LUsername(ConnIX)
                   ELSE
                       MOVE 1 TO LUsername(ConnIX)
                   END-IF
               WHEN "CONNNAME"
                   MOVE ValueString TO ConnectionName(ConnIX)
               WHEN "ESLOGGING"
                   MOVE ValueString(1:1) TO temp
                   PERFORM HexConvert
                   MOVE tempX TO temp2X

                   MOVE ValueString(2:1) TO temp
                   PERFORM HexConvert
                   COMPUTE temp2X = (temp2X * 16) + tempX

                   MOVE ValueString(3:1) TO temp
                   PERFORM HexConvert
                   COMPUTE temp2X = (temp2X * 16) + tempX

                   MOVE ValueString(4:1) TO temp
                   PERFORM HexConvert
                   COMPUTE temp2X = (temp2X * 16) + tempX

                   MOVE temp2X TO EsLoggingLevel(Connix)
                   IF parseErrorFlag not = 0
                       EXIT SECTION
                   END-IF
               WHEN "ISOLEVEL"
                   CALL "CBL_TOUPPER" USING ValueString
                                  BY VALUE FUNCTION LENGTH(ValueString)
                   END-CALL
                   EVALUATE ValueString(1:2)
                   WHEN "UR"
                       SET SqlIsoReadUncommitted(ConnIX) to TRUE
                   WHEN "CR"
                       SET SqlIsoReadCommitted(ConnIX) to TRUE
                   WHEN "RR"
                       SET SqlIsoRepeatableRead(ConnIX) to TRUE
                   WHEN "SZ"
                       SET SqlIsoSerializable(ConnIX) to TRUE
                   WHEN OTHER
                       SET SqlIsoReadCommitted(ConnIX) to TRUE
                   END-EVALUATE
               WHEN SPACES
                   CONTINUE
               WHEN OTHER

                   MOVE 1 TO MsgLen
                   MOVE 1 TO MsgLevel
                   STRING
                       XAi-XA-Resource-Name DELIMITED SPACE
                       ": " DELIMITED SIZE
                       "Invalid open string keyword " 
                               DELIMITED SIZE
                       ParamString DELIMITED SPACE
                   INTO consoleMessage POINTER MsgLen
                   END-STRING
                   SUBTRACT 1 FROM MsgLen
                   IF XAi-SEP-type = sepIsJCL
                     CALL "mfxaLogConsoleMsg" USING 
                        consoleMessage MsgLen 
                        EsLoggingLevel(ConnIX) MsgLevel  
                     END-CALL
                   ELSE
                   CALL "mfxaPutConsoleMsg" USING 
                       consoleMessage MsgLen  
                   END-CALL
                   END-IF

                   MOVE 78-xaer-inval TO ws-open-rc

                   IF 78-CTF-FLAG-LEVEL-ERROR >= ctf-trace-level
                       
                       MOVE EVENT-MSSQL-OPEN-BADOPENSTRING
                           TO ls-trace-event
                       MOVE 78-CTF-FLAG-LEVEL-ERROR  TO ls-trace-level
                       PERFORM trace-odbc-event
                   END-IF
                   GOBACK RETURNING ws-open-rc
                END-EVALUATE

             *> If we've reached the end of the string, drop out
             *> of the perform loop.

                IF     lk-open-string(InnerPosn : 1) = LOW-VALUES
                   EXIT PERFORM
                END-IF

                MOVE InnerPosn TO OuterPosn
                ADD 1 TO OuterPosn
           END-PERFORM
           .

       hexConvert SECTION.
           MOVE 0 TO parseErrorFlag
           *> hopefully this will work in both EBCDIC and ASCII
           IF temp >= '0' AND tempX <= '9'
               MOVE '0' TO temp0
               COMPUTE tempX = tempX - tempX0
           ELSE
               IF tempX >= 'A' AND tempX <= 'F'
                   MOVE 'A' TO tempA
                   COMPUTE tempX = tempX - tempXA
               ELSE
                   MOVE 1 TO parseErrorFlag
               END-IF
           END-IF
           .
           
       SetCurrentConnection SECTION.
           *> Get the connection handle. Scan the array to
           *> locate the resource mgr ID.

           MOVE SPACES TO savedConnectionName
           
           PERFORM VARYING ConnIX FROM 1 BY 1
                UNTIL ConnIX > MAX-CONNECTIONS
                OR ResMgrID(ConnIX) = ws-rmid
           END-PERFORM

           IF ConnIX > MAX-CONNECTIONS
               *> TODO - log an error?
               MOVE 1 TO ConnIX
               EXIT SECTION          
           END-IF
           
           *> If this is a TP SEP that isn't enabled for TP, 
           *> there is no more to do
           IF (XAi-SEP-type = sepIsCICS OR XAi-SEP-type = sepIsIMS
                       OR XAi-SEP-type = sepIsWebServices)
                   AND TxEnabled(ConnIX) = 0
               GOBACK RETURNING XA-OK
           END-IF           
           
           MOVE ConnectionName(ConnIX) TO ThisConnName
           MOVE ConnectionHandle(ConnIX) TO ThisConnHandle

           IF ThisConnName NOT = LOW-VALUES
              EXEC SQL 
                   SET :savedConnectionName = CURRENT CONNECTION
              END-EXEC

              IF 78-CTF-FLAG-LEVEL-INFO >= ctf-trace-level
                  MOVE SPACES TO ls-trace-data-desc
                  STRING "ESODBCXA Set CURRENT Connection(" 
                      savedConnectionName DELIMITED BY "  "
                      ")" DELIMITED BY SPACES
                      INTO ls-trace-data-desc
                  END-STRING
                  MOVE EVENT-ODBC-SETCONN        TO ls-trace-event
                  MOVE 78-CTF-FLAG-LEVEL-INFO TO ls-trace-level
                  PERFORM trace-odbc-event
              END-IF

              IF ThisConnName = savedConnectionName
                   MOVE SPACES TO savedConnectionName              
              ELSE
                   EXEC SQL
                      SET CONNECTION :ThisConnName
                   END-EXEC

                   IF 78-CTF-FLAG-LEVEL-INFO >= ctf-trace-level
                       MOVE SPACES TO ls-trace-data-desc
                       STRING "ESODBCXA Set Connection(" 
                           ThisConnName DELIMITED BY "  "
                           ")" DELIMITED BY SPACES
                           INTO ls-trace-data-desc
                       END-STRING
                       MOVE EVENT-ODBC-SETCONN        TO ls-trace-event
                       MOVE 78-CTF-FLAG-LEVEL-INFO TO ls-trace-level
                       PERFORM trace-odbc-event
                   END-IF
              END-IF
           END-IF

           .

       SetTransIsolationLevel SECTION.

              *> Set the user-specified Transaction Isolation Level
              
              CALL odbcapi "SQLSetConnectAttr" USING
                             BY VALUE ThisConnHandle,
                             BY VALUE SqlTxnIsolation,
                             BY VALUE SqlTxnIsolationValue(ConnIX),
                             BY VALUE 4 SIZE 4
                   RETURNING SqlRtn
              END-CALL

              IF SqlRtn < ZERO

                 *> Set Isolation Level failed. Determine the cause of failure, and
                 *> continue without exiting.
                 
                 MOVE 78-xaer-rmerr TO ws-start-rc ctftrace-rc

                 MOVE SPACES TO MFSQLMESSAGETEXT

                 CALL odbcapi "SQLGetDiagRec" USING
                                BY VALUE SqlHandleDBC
                                BY VALUE ThisConnHandle
                                BY VALUE 1 *> initial record
                                BY REFERENCE SQLSTATE
                                BY REFERENCE SQLCODE
                                BY REFERENCE MFSQLMESSAGETEXT
                                BY VALUE 200 *> length of buffer
                                BY REFERENCE SQLERRML
                      RETURNING SqlRtn
                 END-CALL

                 IF SqlRtn < 0
                    MOVE "Error with SQLGetDiagRec"
                         TO MFSQLMESSAGETEXT
                    MOVE -1 TO SQLCODE
                 END-IF

              ELSE

                 MOVE 78-xa-ok TO ws-start-rc ctftrace-rc

                 MOVE 0 TO SQLCODE SQLSTATE
                 MOVE SPACES TO MFSQLMESSAGETEXT

              END-IF

               IF 78-CTF-FLAG-LEVEL-INFO >= ctf-trace-level
                   MOVE SPACES TO ls-trace-data-desc
                   STRING "ESODBCXA Set Isolation Level (" 
                       ResName(ConnIX) DELIMITED BY "  "
                       ")" DELIMITED BY SPACES
                       INTO ls-trace-data-desc
                   END-STRING
                   MOVE EVENT-ODBC-SETISOLEVEL TO ls-trace-event
                   MOVE 78-CTF-FLAG-LEVEL-INFO TO ls-trace-level
                   PERFORM trace-odbc-event
               END-IF
          .
           
       RestoreApplicationConnection section.
           IF savedConnectionName NOT = SPACES
               EXEC SQL
                   SET CONNECTION :savedConnectionName
               END-EXEC

               IF 78-CTF-FLAG-LEVEL-INFO >= ctf-trace-level
                   MOVE SPACES TO ls-trace-data-desc
                   STRING "ESODBCXA Set Connection restore(" 
                       savedConnectionName DELIMITED BY "  "
                       ")" DELIMITED BY SPACES
                       INTO ls-trace-data-desc
                   END-STRING
                   MOVE EVENT-ODBC-SETCONN        TO ls-trace-event
                   MOVE 78-CTF-FLAG-LEVEL-INFO TO ls-trace-level
                   PERFORM trace-odbc-event
               END-IF

               MOVE SPACES TO savedConnectionName
           END-IF
           .

       GetImpersonatedUser section.
           *> Check for each SEP TYPE
           IF ( DoBATCH(ConnIX) = 0 AND XAi-SEP-type = sepIsJCL ) OR
              ( DoCICS(ConnIX) = 0 AND XAi-SEP-type = sepIsCICS ) OR
              ( DoIMS(ConnIX) = 0 AND XAi-SEP-type = sepIsIMS )
               EXIT SECTION
           END-IF

           IF LUsername(ConnIx) = 0
               IF XAi-current-user = XAi-SEP-initial-user
                 OR XAi-current-user-long-id-txt = SPACES
                   *> No need for impersonation
                   MOVE SPACES TO XAi-current-user-long-id-txt
                   EXIT SECTION
               END-IF

               MOVE XAi-current-user-long-id-txt TO tempLongUserId
               INSPECT tempLongUserId REPLACING TRAILING SPACE BY X"00"
           END-IF

           IF XAi-current-user = XAi-SEP-initial-user
                   OR XAi-current-user = SPACES
               *> No need for impersonation
               MOVE SPACES TO ImpersonatedUser
               EXIT SECTION
           END-IF

           MOVE XAi-current-user TO ImpersonatedUser

           PERFORM VARYING ImpersonatedUserLen FROM 8 BY -1 UNTIL
                   ImpersonatedUserLen = 0 OR
                   ImpersonatedUser(ImpersonatedUserLen:1) NOT = " "
           END-PERFORM
           .

       BeginUserImpersonation section.
           *> Strip trailing spaces from userid

           IF DoImpersonate(ConnIX) = LOW-VALUES
                   OR DatabaseType(ConnIX) NOT = "SQLSERVER"
               EXIT SECTION
           END-IF

           *> Refresh ES info to get current user
           MOVE 2 TO XAi-version
           CALL "mfxaGetRegionInfo" USING BY REFERENCE XA-inquiry
               END-CALL

           *> Check for each SEP TYPE
           IF ( DoBATCH(ConnIX) = 0 AND XAi-SEP-type = sepIsJCL ) OR
              ( DoCICS(ConnIX) = 0 AND XAi-SEP-type = sepIsCICS ) OR
              ( DoIMS(ConnIX) = 0 AND XAi-SEP-type = sepIsIMS )
               EXIT SECTION
           END-IF

           PERFORM GetImpersonatedUser

      *    INITIALIZE sqltext
      *    STRING
      *        "DECLARE @x varbinary(8000); " DELIMITED SIZE
      *        "EXECUTE AS USER = '" DELIMITED SIZE
      *         ImpersonatedUser(1:ImpersonatedUserLen)
      *                        DELIMITED SIZE
      *        "' WITH COOKIE INTO @x; SELECT @x" DELIMITED BY SIZE
      *        INTO sqltext
      *    END-STRING
      *
      *    MOVE SPACES TO MFSQLMESSAGETEXT
      *
      *    EXEC SQL DECLARE CC CURSOR FOR DSQL END-EXEC
      *
      *    EXEC SQL
      *        PREPARE DSQL FROM :sqltext
      *    END-EXEC
      *    IF sqlcode = 0
      *        EXEC SQL OPEN CC END-EXEC
      *        IF SQLCODE = 0
      *            EXEC SQL FETCH CC INTO :COOKIE END-EXEC
      *            EXEC SQL CLOSE CC END-EXEC
      *            MOVE COOKIE TO RevertCookie(ConnIX)
      *        END-IF
      *    END-IF
           INITIALIZE sqltext
           IF LUsername(ConnIx) = 0
               STRING
                 "EXECUTE AS USER = '" DELIMITED SIZE
                 tempLongUserId(1:XAi-current-user-long-id-l)
                 "'" DELIMITED BY SIZE
                 INTO sqltext
               END-STRING
           ELSE
               STRING
                 "EXECUTE AS USER = '" DELIMITED SIZE
                 ImpersonatedUser(1:ImpersonatedUserLen)
                 "'" DELIMITED BY SIZE
                 INTO sqltext
               END-STRING
           END-IF

           MOVE SPACES TO MFSQLMESSAGETEXT

           EXEC SQL EXECUTE IMMEDIATE :sqltext END-EXEC

           IF SQLCODE NOT = 0
               MOVE 0 TO ImpersonatedUserLen
           END-IF

           IF 78-CTF-FLAG-LEVEL-INFO >= ctf-trace-level
               MOVE SPACES TO ls-trace-data-desc
               STRING "ESODBCXA Execute as " XAi-current-user
                  DELIMITED BY SPACE
               INTO ls-trace-data-desc
               END-STRING
               MOVE EVENT-ODBC-EXECUTEAS   TO ls-trace-event
               MOVE 78-CTF-FLAG-LEVEL-INFO TO ls-trace-level
               PERFORM trace-odbc-event
           END-IF
           EXIT SECTION.

       endUserImpersonation section.

           IF DoImpersonate(ConnIX) = LOW-VALUES OR 
              DatabaseType(ConnIX) NOT = "SQLSERVER" OR
              XAi-SEP-type = sepIsWebServices OR
              ( DoBATCH(ConnIX) = 0 AND XAi-SEP-type = sepIsJCL ) OR
              ( DoCICS(ConnIX) = 0 AND XAi-SEP-type = sepIsCICS ) OR
              ( DoIMS(ConnIX) = 0 AND XAi-SEP-type = sepIsIMS )
               EXIT SECTION
           END-IF

           IF IMPERSONATEDUSER = SPACES
               *> No impersonation
               EXIT SECTION
           END-IF

      *    MOVE RevertCookie(ConnIX) To COOKIE
      *    EXEC SQL
      *         REVERT WITH COOKIE :COOKIE
      *    END-EXEC
           MOVE 'REVERT' to sqltext

           EXEC SQL
                EXECUTE IMMEDIATE :sqltext
           END-EXEC

           IF 78-CTF-FLAG-LEVEL-INFO >= ctf-trace-level
               MOVE "ESODBCXA Revert"       TO ls-trace-data-desc
               MOVE EVENT-ODBC-REVERT      TO ls-trace-event
               MOVE 78-CTF-FLAG-LEVEL-INFO TO ls-trace-level
               PERFORM trace-odbc-event
           END-IF

           EXIT SECTION.


           COPY "xapd.cpy" REPLACING ==(XXXXX)== BY ==odbc==.

       trc-odbc-event-openstr-err section.

           *>
           *>  info text
           *>

           MOVE "Error: Open String format is DSN=odbc_dsn"
                & "[,USRPASS=userid.passwd]."
                TO ls-trace-data-desc

           SET ctf-trace-event-ptrs(1) TO 
               ADDRESS OF ls-trace-data-desc

           UNSTRING ls-trace-data-desc DELIMITED BY "  " INTO TmpString
                COUNT ctf-trace-event-lens(1)

           MOVE 78-TRACE-EVENT-TYPE-TEXT TO
                ctf-trace-event-types(1)

           ADD 1 TO ls-trace-data-count

           .

      *-----------------------------------------------------------------
      *    Set up trace data for handling sql errors
      *-----------------------------------------------------------------
       trc-odbc-event-rtn-sqlerr section.

           SET ctf-trace-event-ptrs(1) TO 
               ADDRESS OF ls-trace-data-desc

           UNSTRING ls-trace-data-desc DELIMITED BY "  " INTO TmpString
                COUNT ctf-trace-event-lens(1)

           MOVE 78-TRACE-EVENT-TYPE-TEXT TO
                ctf-trace-event-types(1)

           ADD 1 TO ls-trace-data-count

           *>
           *>  SQL Error message
           *>

           UNSTRING MFSQLMESSAGETEXT DELIMITED BY "   " INTO TmpString
                COUNT ctf-trace-event-lens(2)

           SET ctf-trace-event-ptrs(2) TO 
               ADDRESS OF MFSQLMESSAGETEXT

           MOVE 78-TRACE-EVENT-TYPE-TEXT TO
                ctf-trace-event-types(2)

           ADD 1 TO ls-trace-data-count

           *>
           *>  SQLCODE
           *>

           SET ctf-trace-event-ptrs(3) TO 
               ADDRESS OF SQLCODE

           MOVE length of SQLCODE TO
                ctf-trace-event-lens(3)

           MOVE 78-TRACE-EVENT-TYPE-COMP5S TO
                ctf-trace-event-types(3)

           ADD 1 TO ls-trace-data-count

           *>
           *>  SQLSTATE
           *>

           SET ctf-trace-event-ptrs(4) TO 
               ADDRESS OF SQLSTATE

           MOVE length of SQLSTATE TO
                ctf-trace-event-lens(4)

           MOVE 78-TRACE-EVENT-TYPE-TEXT TO
                ctf-trace-event-types(4)

           ADD 1 TO ls-trace-data-count

           .


       trc-odbc-event-rtncode section.

           *>
           *>  return code from C++ function
           *>

           UNSTRING MFSQLMESSAGETEXT DELIMITED BY "  " INTO TmpString
                COUNT ctf-trace-event-lens(1)

           SET ctf-trace-event-ptrs(1) TO 
               ADDRESS OF MFSQLMESSAGETEXT

           MOVE 78-TRACE-EVENT-TYPE-TEXT TO
                ctf-trace-event-types(1)

           ADD 1 TO ls-trace-data-count

           .

       trc-odbc-event-xacall section.

           SET ctf-trace-event-ptrs(1) TO 
               ADDRESS OF ls-trace-data-desc

           UNSTRING ls-trace-data-desc DELIMITED BY "  " INTO TmpString
                COUNT ctf-trace-event-lens(1)

           MOVE 78-TRACE-EVENT-TYPE-TEXT TO
                ctf-trace-event-types(1)

           ADD 1 TO ls-trace-data-count

           *>
           *>  Return code from XA call
           *>

           SET ctf-trace-event-ptrs(2) TO 
               ADDRESS OF ctftrace-rc

           MOVE length of ctftrace-rc TO
                ctf-trace-event-lens(2)

           MOVE 78-TRACE-EVENT-TYPE-COMP5S TO
                ctf-trace-event-types(2)

           ADD 1 TO ls-trace-data-count

           .

       trc-xid SECTION.

           MOVE SPACES TO TmpString
           IF lk-xid-formatid of lk-xid = -1
               MOVE "Empty XID value" to TmpString
               MOVE 15 to ctf-trace-event-lens(1)
           ELSE
               IF lk-xid-bqual-length of lk-xid > 64 OR
                  lk-xid-gtrid-length of lk-xid > 64
                   MOVE "Invalid XID value" to TmpString
                   MOVE 17 to ctf-trace-event-lens(1)
               ELSE
                   ADD lk-xid-bqual-length of lk-xid to
                     lk-xid-gtrid-length of lk-xid giving
                     ctf-trace-event-lens(1)

                   MOVE lk-xid-value of lk-xid TO TmpString
                   PERFORM VARYING ix FROM 1 BY 1
                     UNTIL ix > ctf-trace-event-lens(1)
                       IF TmpString(ix:1) = low-value
                           MOVE SPACE TO TmpString(ix:1)
                       END-IF
                   END-PERFORM
               END-IF
           END-IF

           SET ctf-trace-event-ptrs(1) TO 
               ADDRESS OF TmpString

           MOVE 78-TRACE-EVENT-TYPE-TEXT TO
                ctf-trace-event-types(1)

           MOVE 1 TO
                cblte-trcevt-data-count OF ctf-trace-event

           CALL "CBL_CTF_TRACE" USING BY VALUE     0
                                      BY REFERENCE ctf-tracer-handle
                                      BY REFERENCE ctf-trace-event
           END-CALL

           IF RETURN-CODE NOT = 0
               DISPLAY "CBL_CTF_TRACE (rc = "
                       return-code
                       ")"
           END-IF
           .

       probeDatabase section.
      $IF DEBUG-PING DEFINED
           add 1 to pingCount
           if pingCount >= pingFailInterval
               move 0 to pingCount
               perform killConnection
           end-if
      $END 
           EVALUATE DatabaseType(ConnIX)
           WHEN "SQLSERVER"
           WHEN "DB2"
               CONTINUE
           WHEN OTHER
           *> this can be any query that will force a server round trip
           *> it doesn't matter if it is a vaid query or not as its
           *> only purpose is to force the driver to notice
           *> when a connection is dead, and the connection is not being 
           *> used by application code at the time this happens
            
               exec 
                   sql select fake 
                   into :dummy 
                   from mf__bogus__ping__table 
               end-exec
           END-EVALUATE
           exit section.

      $IF DEBUG-PING DEFINED
       killConnection section.
           EXEC SQL 
               SET :savedConnectionName = CURRENT CONNECTION
           END-EXEC

           EVALUATE DatabaseType(ConnIX)
           WHEN "SQLSERVER"

               exec sql select @@SPID into :SPID end-exec
           
               exec sql
                   connect :ssuid identified by :sspwd 
                   at xxAUXxxODBC using :ssdb
               end-exec
                     
               exec sql set connection xxAUXxxODBC end-exec
           
               exec sql set autocommit on end-exec
                                            
               string 
                   "kill " delimited size
                   SPID    delimited size 
               into sqlText
           
               exec sql execute immediate :sqlText end-exec
               if sqlcode not  = 0
                   display "FAIL: error killing XA session"
               end-if
           
               exec sql disconnect xxAUXxxODBC end-exec
           WHEN "ORACLE"
               *> make sure you have made V$SESSION visible
               *> Connect as SYS (via SQL*Plus or some other tool) 
               *> Issue: GRANT SELECT ON SYS.V_$SESSION TO <user>
               exec sql select SID, SERIAL# 
                      into :sid, :serial 
                      from v$session
                      where AUDSID = USERENV('SESSIONID')
               end-exec

               exec sql 
                   connect :orauid identified by :orapwd 
                      at xxAUXxxODBC using :oradb
               end-exec

               exec sql set connection xxAUXxxODBC end-exec
                     
               string 
                      "alter system kill session '"
                      sid
                      ","
                      serial
                      "' immediate" 
               into sqlText

               exec sql 
                   execute immediate :sqlText 
               end-exec
               if sqlcode not  = 0
                      display "FAIL: error killing XA session"
               end-if
           
               exec sql 
                   commit work release 
               end-exec
           WHEN "DB2"          
               exec sql select SYSPROC.MON_GET_APPLICATION_HANDLE() 
                          into :AH
                          from SYSIBM.SYSDUMMY1
               end-exec
            
               exec sql
                   connect :db2uid identified by :db2pwd 
                   at xxAUXxxODBC using :db2db
               end-exec

               exec sql set connection xxAUXxxODBC end-exec
           
               move AH to AH-Char
               initialize sqlText
               string 
                      "call SYSPROC.ADMIN_CMD('force application ("
                      AH-Char
                      ")')"
               into sqlText

               exec sql execute immediate :sqlText end-exec
               if sqlcode not  = 0 
                      display "FAIL: error killing XA session"
               end-if
               exec sql disconnect xxAUXxxODBC end-exec
           WHEN "POSTGRESQL"
               exec sql 
                   select pg_backend_pid() 
                   into :SPID 
               end-exec

               exec sql
                  connect :pguid identified by :pgpwd 
                     at xxAUXxxODBC using :pgdb
               end-exec
           
               exec sql set connection xxAUXxxODBC end-exec
           
               exec sql set autocommit on end-exec
                                            
               exec sql
                   select pg_terminate_backend(:SPID)
                   into :pgStatus
               end-exec
               if sqlcode not  = 0 or pgStatus not = 1
                   display "FAIL: error killing XA session"
               end-if
           
               exec sql disconnect xxAUXxxODBC end-exec
           END-EVALUATE

           EXEC SQL
               SET CONNECTION :savedConnectionName
           END-EXEC


           EXIT SECTION.
      $END 
