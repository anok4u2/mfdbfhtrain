
       *> Common WORKING-STORAGE items used by XA switch modules.

       01 ws-xaid                          PIC X(8).
       01 ws-rmid                          PIC S9(9) COMP-5. 

       01 ws-returnCode                    PIC S9(9).
       01 ws-reasonCode                    PIC S9(9).

       01 consoleMessage                   PIC X(200).
       01 MsgLen                           PIC S9(4) COMP-5.
       01 MsgLevel                         pic x(2) comp-x.

       01 OuterPosn                        PIC 9(4) VALUE 0.
       01 InnerPosn                        PIC 9(4) VALUE 0.

       01 JobNumber                        PIC 9(5) VALUE 0.

       01 ConnIx                  PIC S9(4) COMP-5 VALUE 1.
       01 ParmIx                  PIC S9(4) COMP-5.
       
      *01  sepType                         PIC X COMP-X VALUE 0.
           78  sepTypeUnknown              VALUE 0.
           78  sepIsCICS                   VALUE 1.
           78  sepIsIMS                    VALUE 2.
           78  sepIsJCL                    VALUE 3.
           78  sepIsWebServices            VALUE 4.
       78  sepIsAdmin                  VALUE 5.
       78  sepIsOther                  VALUE 6.

       01  XA-inquiry.
           03  XAi-length                  PIC X(4) COMP-5.
           03  XAi-version                 PIC X(2) COMP-5.
           03  filler                      PIC x(1) COMP-X.        
           03  XAi-SEP-type                PIC x(1) COMP-X.
           03  XAi-Region-Name             PIC x(8).
           03  XAi-SEP-initial-user        PIC X(8).
           03  XAi-XA-Resource-Name        PIC X(8).
           03  XAi-GUID-ptr                POINTER.
           03  XAi-computer-name-ptr       POINTER.
           03  XAi-computer-name-ptr-X redefines XAi-computer-name-ptr
      $IF P64 SET
                                           PIC S9(18) COMP-5.
      $ELSE
                                           PIC S9(8) COMP-5.
      $END
           03  XAi-current-user            PIC x(8).
           03  XAi-current-user-long-id.
               05 XAi-current-user-long-id-l    pic x comp-x.
               05 XAi-current-user-long-id-txt  pic x(255).
           03 XAi-job-transaction-term.
               05 XAi-transid              PIC x(4).
               05 XAi--termid              PIC x(4).
           03 XAi-step-name-netname        PIC x(8).
           03 XAi-job-proc-name            PIC x(8).
           03 XAi-job-task-number          PIC x(4) comp-5.

       01  Xai-retcode                     PIC S9(4) COMP-5.

       01 CustomizationExitRoutine     PROCEDURE-POINTER.

       01 OracleCustomExitRoutine          PROCEDURE-POINTER.

       01 DB2CustomExitRoutine             PROCEDURE-POINTER.
       
       01 ImpersonatedUserLen              BINARY-LONG VALUE 5.
       
       01 UserId                           PIC X(8).
       01 tempUserId                       PIC X(10).
       01 tempLongUserId                   PIC X(255).
       01 UserIdLen                        PIC S9(8) COMP-5.

       01  abend-type                  PIC  X(04) COMP-5.
       01  abend-code                  PIC  X(04) COMP-5.



       01 XAResName                        PIC X(8).

       01 EnvValue                         PIC X(256) VALUE SPACES.
       
       01 ws-boolCICSEOT                   PIC X(4) COMP-5.

       01 SEP-initial-user                 PIC X(8).

       01 TmpString                        PIC X(200) VALUE SPACES.
       
       01 isInitialised                    PIC 99 COMP-X VALUE 0.

       *> Macro delimiter character
       01  MacroDelim                      PIC X.

       *> XA Open String to be used

       01 ws-open-string                   PIC X(255).
       01 uc-open-string                   PIC X(255).
       01 tmp-open-string                  PIC X(255).

      *
      * xa-() return codes (resource manager reports to transaction manager)
      * 
       01   XA-RBBASE		binary-long value 100.			*> The inclusive lower bound of the rollback codes 
       01   XA-RBROLLBACK	binary-long value 100.	        *> The rollback was caused by an unspecified reason 
       01   XA-RBCOMMFAIL	binary-long value 101.	        *> The rollback was caused by a communication failure 
       01   XA-RBDEADLOCK	binary-long value 102.	        *> A deadlock was detected 
       01   XA-RBINTEGRITY	binary-long value 103.      	*> A condition that violates the integrity of the resources was detected 
       01   XA-RBOTHER		binary-long value 104.	        *> The resource manager rolled back the transaction branch for a reason not on this list 
       01   XA-RBPROTO		binary-long value 105.	        *> A protocol error occurred in the resource manager 
       01   XA-RBTIMEOUT	binary-long value 106.	        *> A transaction branch took too long 
       01   XA-RBTRANSIENT	binary-long value 107.	        *> May retry the transaction branch 
       01   XA-RBEND		binary-long value 107.          *> The inclusive upper bound of the rollback codes 

       01   XA-NOMIGRATE	binary-long value 9.			*> resumption must occur where suspension occurred 
       01   XA-HEURHAZ		binary-long value 8.			*> the transaction branch may have been heuristically completed 
       01   XA-HEURCOM		binary-long value 7.			*> the transaction branch has been heuristically committed 
       01   XA-HEURRB		binary-long value 6.			*> the transaction branch has been heuristically rolled back 
       01   XA-HEURMIX		binary-long value 5.			*> the transaction branch has been heuristically committed and rolled back 
       01   XA-RETRY		binary-long value 4.			*> routine returned with no effect and may be re-issued 
       01   XA-RDONLY		binary-long value 3.			*> the transaction branch was read-only and has been committed 
       01   XA-OK			binary-long value 0.			*> normal execution 
       01   XAER-ASYNC		binary-long value -2.			*> asynchronous operation already outstanding 
       01   XAER-RMERR		binary-long value -3.			*> a resource manager error occurred in the transaction branch 
       01   XAER-NOTA		binary-long value -4.			*> the XID is not valid 
       01   XAER-INVAL		binary-long value -5.			*> invalid arguments were given 
       01   XAER-PROTO		binary-long value -6.			*> routine invoked in an improper context 
       01   XAER-RMFAIL		binary-long value -7.			*> resource manager unavailable 
       01   XAER-DUPID		binary-long value -8.			*> the XID already exists 
       01   XAER-OUTSIDE	binary-long value -9.			*> resource manager doing work outside 

      *
      * NOTE: Used 01 binary-long value rather than 78 to ensure we get int size when passing to xa routines
      * Flag definitions for the RM switch
      *
       01  TMNOFLAGS        binary-long value 0.          *> no resource manager features selected
       01  TMREGISTER       binary-long value 1.          *> resource manager dynamically registers
       01  TMNOMIGRATE      binary-long value 2.           *> resource manager does not support association migration
       01  TMUSEASYNC       binary-long value 4.           *> resource manager supports asynchronous operations

      *
      * Flag definitions for xa_ and ax_ routines
      *
      * use TMNOFLAGS, defined above, when not specifying other flags
      *
       01   TMASYNC			pic x(4) comp-5
                                        value H"80000000".	*> perform routine asynchronously
       01   TMONEPHASE		binary-long value H"40000000".	*> caller is using one-phase commit optimisation
       01   TMFAIL			binary-long value H"20000000".	*> dissociates caller and marks transaction branch rollback-only
       01   TMNOWAIT		binary-long value H"10000000".	*> return if blocking condition exists
       01   TMRESUME		binary-long value H"08000000".	*> caller is resuming association with suspended transaction branch
       01   TMSUCCESS		binary-long value H"04000000".	*> dissociate caller from transaction branch
       01   TMSUSPEND		binary-long value H"02000000".	*> caller is suspending, not ending, association
       01   TMSTARTRSCAN	binary-long value H"01000000".	*> start a recovery scan
       01   TMENDRSCAN		binary-long value H"00800000".	*> end a recovery scan
       01   TMMULTIPLE		binary-long value H"00400000".	*> wait for any asynchronous operation
       01   TMJOIN			binary-long value H"00200000".	*> caller is joining existing transaction branch
       01   TMMIGRATE		binary-long value H"00100000".	*> caller intends to perform migration
      *
      * ax_() return codes (transaction manager reports to resource manager)
      *
       01   TM-JOIN			binary-long value 2.			*> caller is joining existing transaction branch
       01   TM-RESUME		binary-long value 1.			*> caller is resuming association with suspended transaction branch
       01   TM-OK			binary-long value 0.			*> normal execution
       01   TMER-TMERR		binary-long value -1.			*> an error occurred in the transaction manager
       01   TMER-INVAL		binary-long value -2.			*> invalid arguments were given
       01   TMER-PROTO		binary-long value -3.			*> routine invoked in an improper context

       
       01 mfesxa-switch-struct.
          03 mfesxa-RM-name                PIC X(32).
      $IF XAPLATFORM = "UNIX"
      $IF P64 SET
          03 mfesxa-flags                  PIC S9(18) COMP-5.
          03 mfesxa-version                PIC S9(18) COMP-5.
      $ELSE
          03 mfesxa-flags                  PIC S9(9) COMP-5.
          03 mfesxa-version                PIC S9(9) COMP-5.
      $END
      $ELSE
          03 mfesxa-flags                  PIC S9(9) COMP-5.
          03 mfesxa-version                PIC S9(9) COMP-5.
      $END
          03 mfesxa-open-entry             PROCEDURE-POINTER.
          03 mfesxa-close-entry            PROCEDURE-POINTER.
          03 mfesxa-start-entry            PROCEDURE-POINTER.
          03 mfesxa-end-entry              PROCEDURE-POINTER.
          03 mfesxa-rollback-entry         PROCEDURE-POINTER.
          03 mfesxa-prepare-entry          PROCEDURE-POINTER.
          03 mfesxa-commit-entry           PROCEDURE-POINTER.
          03 mfesxa-recover-entry          PROCEDURE-POINTER.
          03 mfesxa-forget-entry           PROCEDURE-POINTER.
          03 mfesxa-complete-entry         PROCEDURE-POINTER.
          03 mfesxa-ping-entry             PROCEDURE-POINTER.
      $IF XAHOOK DEFINED
          03 mfesxa-hook-entry             PROCEDURE-POINTER.
      $END XAHOOK DEFINED
          03 mfesxa-1phase.
             05 mfesxa-1phase-flags            PIC X(8).
             05 mfesxa-1phase-connect-entry    PROCEDURE-POINTER.
             05 mfesxa-1phase-disconnect-entry PROCEDURE-POINTER.
             05 mfesxa-1phase-commit-entry     PROCEDURE-POINTER.
             05 mfesxa-1phase-rollback-entry   PROCEDURE-POINTER.

       *> Return codes for XA calls

       01 ws-return-flags.
          03 ws-open-rc                    PIC S9(9) COMP-5 VALUE 0.
          03 ws-close-rc                   PIC S9(9) COMP-5 VALUE 0.
          03 ws-start-rc                   PIC S9(9) COMP-5 VALUE 0.
          03 ws-end-rc                     PIC S9(9) COMP-5 VALUE 0.
          03 ws-rollback-rc                PIC S9(9) COMP-5 VALUE 0.
          03 ws-prepare-rc                 PIC S9(9) COMP-5 VALUE 0.
          03 ws-commit-rc                  PIC S9(9) COMP-5 VALUE 0.
          03 ws-recover-rc                 PIC S9(9) COMP-5 VALUE 0.
          03 ws-forget-rc                  PIC S9(9) COMP-5 VALUE 0.
          03 ws-complete-rc                PIC S9(9) COMP-5 VALUE 0.
          03 ws-register-rc                PIC S9(9) COMP-5 VALUE 0.
          03 ws-open-rc-save               PIC S9(9) COMP-5 VALUE 0.
          03 ws-generic-rc                 PIC S9(9) COMP-5 VALUE 0.
          03 ws-open-is-2-phase            PIC X  VALUE 'N'.

       01 ws-connect-status          pic x comp-x.
           88 ws-connect-ok-88       value 0.
           88 ws-connect-fail-88     value 1.

       *> Return code values as defined by X/OPEN

       78 78-xa-ok                         value  0.
       78 78-xaer-rmerr                    value -3.
       78 78-xaer-nota                     value -4.
       78 78-xaer-inval                    value -5.
       78 78-xaer-rmfail                   value -7.
       78 78-xa-rbrollback                 value 100.
       78 78-xa-rbcommfail                 value 101.
       78 78-xa-rbother                    value 104.
       78 78-xa-rbtimeout                  value 106.

       *> Consolidated Tracing Facility

       78  MAX-TRACE-DATA              VALUE 92.
       78  THIS-COMPONENT              VALUE "MF.ESXA".

      $if MFDBFH-SUPPORT defined
      *----------------------------------------------------------------
      *    MFDBFH-related data items
      *----------------------------------------------------------------

      *    Connection registration
      *
      *     Flags bit settings:
      *     
      *         bits 0-7        Database provider
      *             Unknown         = 0x00000000,
      *             SqlServer       = 0x00000001,
      *             PostgreSQL      = 0x00000002,
      *             Db2             = 0x00000003,
      *
      *             DbProviderMask  = 0x000000FF,
      *
      *         bits 8-31       Reserved for future use (must be 0)
       78  78-MFDBFH-CONN-REG-FLAGS-NONE           value h'00000000'.

       78  78-MFDBFH-CONN-REG-FLAGS-UNKNOWN        value h'00000000'.
       78  78-MFDBFH-CONN-REG-FLAGS-SQLSERVER      value h'00000001'.
       78  78-MFDBFH-CONN-REG-FLAGS-POSTGRESQL     value h'00000002'.
       78  78-MFDBFH-CONN-REG-FLAGS-DB2            value h'00000003'.
       78  78-MFDBFH-CONN-REG-FLAGS-ORACLE         value h'00000004'.

       78  78-MFDBFH-CONN-REG-RC-SUCCESS           value 0.
       78  78-MFDBFH-CONN-REG-RC-INV-CONN-TYPE     value 1.
       78  78-MFDBFH-CONN-REG-RC-INV-CONN          value 2.
       78  78-MFDBFH-CONN-REG-RC-NO-CONFIG         value 3.
       78  78-MFDBFH-CONN-REG-RC-NOT-DATASTORE     value 4.
       78  78-MFDBFH-CONN-REG-RC-NOT-FOUND         value 5.

       78  78-MFDBFH-CONN-REG-TYPE-ODBC            value 0.
       78  78-MFDBFH-CONN-REG-TYPE-OCI             value 1.

       01.
           03  ws-mfdbfh-connection-reg-flags      binary-long value 78-MFDBFH-CONN-REG-FLAGS-NONE.
           03  ws-mfdbfh-connection-dereg-pptr     procedure-pointer value null.
           03  ws-mfdbfh-connection-reg-pptr       procedure-pointer value null.
           03  ws-mfdbfh-connection-reg-cred-pptr  procedure-pointer value null.
           03  ws-mfdbfh-invocation-observer-pptr  procedure-pointer value null.
           03  ws-mfdbfh-pptr                      procedure-pointer value null.
           03  ws-mfdbfh-rc                        binary-long.
           03  ws-mfdbfh-rc-display                pic s9(9).
      $end MFDBFH-SUPPORT defined

      *----------------------------------------------------------------
      *    THIS_COMPONENT's tracer properties
      *----------------------------------------------------------------
       78  PROPID-ALL                  VALUE 1.

       78  PROPNAME-ALL                VALUE "all".
       78  PROPNAME-DB2                VALUE "db2".
       78  PROPNAME-MSSQL              VALUE "mssql".
       78  PROPNAME-OCI                VALUE "oci".
       78  PROPNAME-ODBC               VALUE "odbc".
       78  PROPNAME-ORA                VALUE "ora".
       78  PROPNAME-PGSQL              VALUE "pgsql".
       78  PROPNAME-XDB                VALUE "xdb".
       78  PROPNAME-XDBOPC             VALUE "xdbopc".
       78  PROPNAME-MSSQL-OPEN         VALUE "ESMSSQL xa_open".
       78  PROPNAME-MSSQL-START        VALUE "ESMSSQL xa_start".
       78  PROPNAME-MSSQL-COMMIT       VALUE "ESMSSQL xa_commit".
       78  PROPNAME-MSSQL-ROLLBACK     VALUE "ESMSSQL xa_rollback".
       78  PROPNAME-MSSQL-CLOSE        VALUE "ESMSSQL xa_close".
       78  PROPNAME-MSSQL-AXREG        VALUE "ESMSSQL ax_reg".
       78  PROPNAME-MSSQL-PREPARE      VALUE "ESMSSQL xa_prepare".
       78  PROPNAME-ODBC-OPEN          VALUE "ESODBCXA open".
       78  PROPNAME-ODBC-START         VALUE "ESODBCXA start".
       78  PROPNAME-ODBC-COMMIT        VALUE "ESODBCXA commit".
       78  PROPNAME-ODBC-ROLLBACK      VALUE "ESODBCXA rollback".
       78  PROPNAME-ODBC-CLOSE         VALUE "ESODBCXA close".
       78  PROPNAME-ODBC-SYNCPOINT     VALUE "ESODBCXA syncpoint".
       78  PROPNAME-ODBC-RESETCONN     VALUE "ESODBCXA resetconn".
       78  PROPNAME-ODBC-EXECUTEAS     VALUE "ESODBCXA executeas".
       78  PROPNAME-ODBC-REVERT        VALUE "ESODBCXA revert".
       78  PROPNAME-ODBC-XATRIGGER     VALUE "ESODBCXA xatrigger".
       78  PROPNAME-ODBC-SYNCROLLBK    VALUE "ESODBCXA syncrollbk".
       78  PROPNAME-ODBC-RSETHSTCON    VALUE "ESODBCXA rsethstconn".
       78  PROPNAME-ODBC-SETCONN       VALUE "ESODBCXA setconn".
       78  PROPNAME-DB2-DB2LOAD        VALUE "ESDB2XA db2load".
       78  PROPNAME-DB2-START          VALUE "ESDB2XA xa_start".
       78  PROPNAME-DB2-END            VALUE "ESDB2XA xa_end".
       78  PROPNAME-DB2-OPEN           VALUE "ESDB2XA xa_open".
       78  PROPNAME-DB2-CLOSE          VALUE "ESDB2XA xa_close".
       78  PROPNAME-DB2-COMMIT         VALUE "ESDB2XA xa_commit".
       78  PROPNAME-DB2-ROLLBACK       VALUE "ESDB2XA xa_rollback".
       78  PROPNAME-DB2-PREPARE        VALUE "ESDB2XA xa_prepare".
       78  PROPNAME-DB2-AXREG          VALUE "ESDB2XA ax_reg".
       78  PROPNAME-DB2OPC-OPEN        VALUE "ESDB2OPC connect".
       78  PROPNAME-DB2OPC-COMMIT      VALUE "ESDB2OPC commit".
       78  PROPNAME-DB2OPC-ROLLBACK    VALUE "ESDB2OPC rollback".
       78  PROPNAME-DB2OPC-CLOSE       VALUE "ESDB2OPC disconnect".
       78  PROPNAME-DB2OPC-PREOPEN     VALUE "ESDB2OPC preconnect".
       78  PROPNAME-DB2OPC-PRECOMMIT   VALUE "ESDB2OPC precommit".
       78  PROPNAME-DB2OPC-PREROLLBACK VALUE "ESDB2OPC prerollbck".
       78  PROPNAME-DB2OPC-PRECLOSE    VALUE "ESDB2OPC predisconn".
       78  PROPNAME-ORA-CASLOAD        VALUE "ESORAXA casload".
       78  PROPNAME-ORA-ORALOAD        VALUE "ESORAXA oraload".
       78  PROPNAME-ORA-XAOSW          VALUE "ESORAXA xaosw".
       78  PROPNAME-ORA-START          VALUE "ESORAXA xa_start".
       78  PROPNAME-ORA-END            VALUE "ESORAXA xa_end".
       78  PROPNAME-ORA-OPEN           VALUE "ESORAXA xa_open".
       78  PROPNAME-ORA-CLOSE          VALUE "ESORAXA xa_close".
       78  PROPNAME-ORA-COMMIT         VALUE "ESORAXA xa_commit".
       78  PROPNAME-ORA-ROLLBACK       VALUE "ESORAXA xa_rollback".
       78  PROPNAME-ORA-PREPARE        VALUE "ESORAXA xa_prepare".
       78  PROPNAME-ORA-RECOVER        VALUE "ESORAXA xa_recover".
       78  PROPNAME-ORA-AXREG          VALUE "ESORAXA ax_reg".
       78  PROPNAME-ORA-XAPING         VALUE "ESORAXA xa_ping".
       78  PROPNAME-ORAOPC-OPEN        VALUE "ESORAOPC connect".
       78  PROPNAME-ORAOPC-COMMIT      VALUE "ESORAOPC commit".
       78  PROPNAME-ORAOPC-ROLLBACK    VALUE "ESORAOPC rollback".
       78  PROPNAME-ORAOPC-CLOSE       VALUE "ESORAOPC disconnect".
       78  PROPNAME-ORAOPC-PREOPEN     VALUE "ESORAOPC preconnect".
       78  PROPNAME-ORAOPC-PRECOMMIT   VALUE "ESORAOPC precommit".
       78  PROPNAME-ORAOPC-PREROLLBACK VALUE "ESORAOPC prerollbck".
       78  PROPNAME-ORAOPC-PRECLOSE    VALUE "ESORAOPC predisconn".
       78  PROPNAME-OCI-CASLOAD        VALUE "ESOCIXA casload".
       78  PROPNAME-OCI-OCILOAD        VALUE "ESOCIXA oraload".
       78  PROPNAME-OCI-XAOSW          VALUE "ESOCIXA xaosw".
       78  PROPNAME-OCI-START          VALUE "ESOCIXA xa_start".
       78  PROPNAME-OCI-END            VALUE "ESOCIXA xa_end".
       78  PROPNAME-OCI-OPEN           VALUE "ESOCIXA xa_open".
       78  PROPNAME-OCI-BIND           VALUE "ESOCIXA conn bind".
       78  PROPNAME-OCI-UNBIND         VALUE "ESOCIXA conn unbind".
       78  PROPNAME-OCI-CLOSE          VALUE "ESOCIXA xa_close".
       78  PROPNAME-OCI-COMMIT         VALUE "ESOCIXA xa_commit".
       78  PROPNAME-OCI-ROLLBACK       VALUE "ESOCIXA xa_rollback".
       78  PROPNAME-OCI-PREPARE        VALUE "ESOCIXA xa_prepare".
       78  PROPNAME-OCI-XATRIGGER      VALUE "ESOCIXA xatrigger".
       78  PROPNAME-OCI-AXREG          VALUE "ESOCIXA ax_reg".
       78  PROPNAME-OCI-SYNCPOINT      VALUE "ESOCIXA syncpoint".
       78  PROPNAME-OCI-RESETCONN      VALUE "ESOCIXA resetconn".
       78  PROPNAME-OCIOPC-OPEN        VALUE "ESOCIOPC connect".
       78  PROPNAME-OCIOPC-COMMIT      VALUE "ESOCIOPC commit".
       78  PROPNAME-OCIOPC-ROLLBACK    VALUE "ESOCIOPC rollback".
       78  PROPNAME-OCIOPC-CLOSE       VALUE "ESOCIOPC disconnect".
       78  PROPNAME-OCIOPC-PREOPEN     VALUE "ESOCIOPC preconnect".
       78  PROPNAME-OCIOPC-PRECOMMIT   VALUE "ESOCIOPC precommit".
       78  PROPNAME-OCIOPC-PREROLLBACK VALUE "ESOCIOPC prerollbck".
       78  PROPNAME-OCIOPC-PRECLOSE    VALUE "ESOCIOPC predisconn".
       78  PROPNAME-XDB-XDBLOAD        VALUE "ESXDBXA xdbload".
       78  PROPNAME-XDB-RESETCONN      VALUE "ESXDBXA resetconn".
       78  PROPNAME-XDB-START          VALUE "ESXDBXA xa_start".
       78  PROPNAME-XDB-END            VALUE "ESXDBXA xa_end".
       78  PROPNAME-XDB-OPEN           VALUE "ESXDBXA xa_open".
       78  PROPNAME-XDB-CLOSE          VALUE "ESXDBXA xa_close".
       78  PROPNAME-XDB-COMMIT         VALUE "ESXDBXA xa_commit".
       78  PROPNAME-XDB-ROLLBACK       VALUE "ESXDBXA xa_rollback".
       78  PROPNAME-XDB-PREPARE        VALUE "ESXDBXA xa_prepare".
       78  PROPNAME-XDBOPC-OPEN        VALUE "ESXDBOPC connect".
       78  PROPNAME-XDBOPC-COMMIT      VALUE "ESXDBOPC commit".
       78  PROPNAME-XDBOPC-ROLLBACK    VALUE "ESXDBOPC rollback".
       78  PROPNAME-XDBOPC-CLOSE       VALUE "ESXDBOPC disconnect".
       78  PROPNAME-XDBOPC-PREOPEN     VALUE "ESXDBOPC preconnect".
       78  PROPNAME-XDBOPC-PRECOMMIT   VALUE "ESXDBOPC precommit".
       78  PROPNAME-XDBOPC-PREROLLBACK VALUE "ESXDBOPC prerollbck".
       78  PROPNAME-XDBOPC-PRECLOSE    VALUE "ESXDBOPC predisconn".
       78  PROPNAME-PGSQL-OPEN          VALUE "ESPGSQLXA open".
       78  PROPNAME-PGSQL-START         VALUE "ESPGSQLXA start".
       78  PROPNAME-PGSQL-COMMIT        VALUE "ESPGSQLXA commit".
       78  PROPNAME-PGSQL-ROLLBACK      VALUE "ESPGSQLXA rollback".
       78  PROPNAME-PGSQL-CLOSE         VALUE "ESPGSQLXA close".
       78  PROPNAME-PGSQL-SYNCPOINT     VALUE "ESPGSQLXA syncpoint".
       78  PROPNAME-PGSQL-RESETCONN     VALUE "ESPGSQLXA resetconn".
       78  PROPNAME-PGSQL-EXECUTEAS     VALUE "ESPGSQLXA executeas".
       78  PROPNAME-PGSQL-REVERT        VALUE "ESPGSQLXA revert".
       78  PROPNAME-PGSQL-XATRIGGER     VALUE "ESPGSQLXA xatrigger".
       78  PROPNAME-PGSQL-SYNCROLLBK    VALUE "ESPGSQLXA syncrollbk".
       78  PROPNAME-PGSQL-RSTHSTCON    VALUE "ESPGSQLXA rsthstconn".
       78  PROPNAME-PGSQL-SETCONN       VALUE "ESPGSQLXA setconn".

       01  ws-prop-names.
           03           PIC X(20) VALUE PROPNAME-ALL.
           03           PIC X(20) VALUE PROPNAME-DB2.
           03           PIC X(20) VALUE PROPNAME-MSSQL.
           03           PIC X(20) VALUE PROPNAME-OCI.
           03           PIC X(20) VALUE PROPNAME-ODBC.
           03           PIC X(20) VALUE PROPNAME-ORA.
           03           PIC X(20) VALUE PROPNAME-PGSQL.
           03           PIC X(20) VALUE PROPNAME-XDB.
           03           PIC X(20) VALUE PROPNAME-XDBOPC.
           03           PIC X(20) VALUE PROPNAME-MSSQL-OPEN.
           03           PIC X(20) VALUE PROPNAME-MSSQL-START.
           03           PIC X(20) VALUE PROPNAME-MSSQL-COMMIT.
           03           PIC X(20) VALUE PROPNAME-MSSQL-ROLLBACK.
           03           PIC X(20) VALUE PROPNAME-MSSQL-CLOSE.
           03           PIC X(20) VALUE PROPNAME-MSSQL-AXREG.
           03           PIC X(20) VALUE PROPNAME-MSSQL-PREPARE.
           03           PIC X(20) VALUE PROPNAME-ODBC-OPEN.
           03           PIC X(20) VALUE PROPNAME-ODBC-START.
           03           PIC X(20) VALUE PROPNAME-ODBC-COMMIT.
           03           PIC X(20) VALUE PROPNAME-ODBC-ROLLBACK.
           03           PIC X(20) VALUE PROPNAME-ODBC-CLOSE.
           03           PIC X(20) VALUE PROPNAME-ODBC-SYNCPOINT.
           03           PIC X(20) VALUE PROPNAME-ODBC-RESETCONN.
           03           PIC X(20) VALUE PROPNAME-ODBC-EXECUTEAS.
           03           PIC X(20) VALUE PROPNAME-ODBC-REVERT.
           03           PIC X(20) VALUE PROPNAME-ODBC-XATRIGGER.
           03           PIC X(20) VALUE PROPNAME-ODBC-SYNCROLLBK.
           03           PIC X(20) VALUE PROPNAME-ODBC-RSETHSTCON.
           03           PIC X(20) VALUE PROPNAME-ODBC-SETCONN.
           03           PIC X(20) VALUE PROPNAME-DB2-DB2LOAD.
           03           PIC X(20) VALUE PROPNAME-DB2-START.
           03           PIC X(20) VALUE PROPNAME-DB2-END.
           03           PIC X(20) VALUE PROPNAME-DB2-OPEN.
           03           PIC X(20) VALUE PROPNAME-DB2-CLOSE.
           03           PIC X(20) VALUE PROPNAME-DB2-COMMIT.
           03           PIC X(20) VALUE PROPNAME-DB2-ROLLBACK.
           03           PIC X(20) VALUE PROPNAME-DB2-PREPARE.
           03           PIC X(20) VALUE PROPNAME-DB2-AXREG.
           03           PIC X(20) VALUE PROPNAME-DB2OPC-OPEN.
           03           PIC X(20) VALUE PROPNAME-DB2OPC-COMMIT.
           03           PIC X(20) VALUE PROPNAME-DB2OPC-ROLLBACK.
           03           PIC X(20) VALUE PROPNAME-DB2OPC-CLOSE.
           03           PIC X(20) VALUE PROPNAME-DB2OPC-PREOPEN.
           03           PIC X(20) VALUE PROPNAME-DB2OPC-PRECOMMIT.
           03           PIC X(20) VALUE PROPNAME-DB2OPC-PREROLLBACK.
           03           PIC X(20) VALUE PROPNAME-DB2OPC-PRECLOSE.
           03           PIC X(20) VALUE PROPNAME-ORA-CASLOAD.
           03           PIC X(20) VALUE PROPNAME-ORA-ORALOAD.
           03           PIC X(20) VALUE PROPNAME-ORA-XAOSW.
           03           PIC X(20) VALUE PROPNAME-ORA-START.
           03           PIC X(20) VALUE PROPNAME-ORA-END.
           03           PIC X(20) VALUE PROPNAME-ORA-OPEN.
           03           PIC X(20) VALUE PROPNAME-ORA-CLOSE.
           03           PIC X(20) VALUE PROPNAME-ORA-COMMIT.
           03           PIC X(20) VALUE PROPNAME-ORA-ROLLBACK.
           03           PIC X(20) VALUE PROPNAME-ORA-PREPARE.
           03           PIC X(20) VALUE PROPNAME-ORA-RECOVER.
           03           PIC X(20) VALUE PROPNAME-ORA-AXREG.
           03           PIC X(20) VALUE PROPNAME-ORA-XAPING.
           03           PIC X(20) VALUE PROPNAME-ORAOPC-OPEN.
           03           PIC X(20) VALUE PROPNAME-ORAOPC-COMMIT.
           03           PIC X(20) VALUE PROPNAME-ORAOPC-ROLLBACK.
           03           PIC X(20) VALUE PROPNAME-ORAOPC-CLOSE.
           03           PIC X(20) VALUE PROPNAME-ORAOPC-PREOPEN.
           03           PIC X(20) VALUE PROPNAME-ORAOPC-PRECOMMIT.
           03           PIC X(20) VALUE PROPNAME-ORAOPC-PREROLLBACK.
           03           PIC X(20) VALUE PROPNAME-ORAOPC-PRECLOSE.
           03           PIC X(20) VALUE PROPNAME-OCI-CASLOAD.
           03           PIC X(20) VALUE PROPNAME-OCI-OCILOAD.
           03           PIC X(20) VALUE PROPNAME-OCI-XAOSW.
           03           PIC X(20) VALUE PROPNAME-OCI-START.
           03           PIC X(20) VALUE PROPNAME-OCI-END.
           03           PIC X(20) VALUE PROPNAME-OCI-OPEN.
           03           PIC X(20) VALUE PROPNAME-OCI-CLOSE.
           03           PIC X(20) VALUE PROPNAME-OCI-COMMIT.
           03           PIC X(20) VALUE PROPNAME-OCI-ROLLBACK.
           03           PIC X(20) VALUE PROPNAME-OCI-PREPARE.
           03           PIC X(20) VALUE PROPNAME-OCI-XATRIGGER.
           03           PIC X(20) VALUE PROPNAME-OCI-BIND.
           03           PIC X(20) VALUE PROPNAME-OCI-UNBIND.
           03           PIC X(20) VALUE PROPNAME-OCI-AXREG.
           03           PIC X(20) VALUE PROPNAME-OCI-SYNCPOINT.
           03           PIC X(20) VALUE PROPNAME-OCI-RESETCONN.
           03           PIC X(20) VALUE PROPNAME-OCIOPC-OPEN.
           03           PIC X(20) VALUE PROPNAME-OCIOPC-COMMIT.
           03           PIC X(20) VALUE PROPNAME-OCIOPC-ROLLBACK.
           03           PIC X(20) VALUE PROPNAME-OCIOPC-CLOSE.
           03           PIC X(20) VALUE PROPNAME-OCIOPC-PREOPEN.
           03           PIC X(20) VALUE PROPNAME-OCIOPC-PRECOMMIT.
           03           PIC X(20) VALUE PROPNAME-OCIOPC-PREROLLBACK.
           03           PIC X(20) VALUE PROPNAME-OCIOPC-PRECLOSE.
           03           PIC X(20) VALUE PROPNAME-XDB-XDBLOAD.
           03           PIC X(20) VALUE PROPNAME-XDB-RESETCONN.
           03           PIC X(20) VALUE PROPNAME-XDB-START.
           03           PIC X(20) VALUE PROPNAME-XDB-END.
           03           PIC X(20) VALUE PROPNAME-XDB-OPEN.
           03           PIC X(20) VALUE PROPNAME-XDB-CLOSE.
           03           PIC X(20) VALUE PROPNAME-XDB-COMMIT.
           03           PIC X(20) VALUE PROPNAME-XDB-ROLLBACK.
           03           PIC X(20) VALUE PROPNAME-XDB-PREPARE.
           03           PIC X(20) VALUE PROPNAME-XDBOPC-OPEN.
           03           PIC X(20) VALUE PROPNAME-XDBOPC-COMMIT.
           03           PIC X(20) VALUE PROPNAME-XDBOPC-ROLLBACK.
           03           PIC X(20) VALUE PROPNAME-XDBOPC-CLOSE.
           03           PIC X(20) VALUE PROPNAME-XDBOPC-PREOPEN.
           03           PIC X(20) VALUE PROPNAME-XDBOPC-PRECOMMIT.
           03           PIC X(20) VALUE PROPNAME-XDBOPC-PREROLLBACK.
           03           PIC X(20) VALUE PROPNAME-XDBOPC-PRECLOSE.
           03           PIC X(20) VALUE PROPNAME-PGSQL-OPEN.
           03           PIC X(20) VALUE PROPNAME-PGSQL-START.
           03           PIC X(20) VALUE PROPNAME-PGSQL-COMMIT.
           03           PIC X(20) VALUE PROPNAME-PGSQL-ROLLBACK.
           03           PIC X(20) VALUE PROPNAME-PGSQL-CLOSE.
           03           PIC X(20) VALUE PROPNAME-PGSQL-SYNCPOINT.
           03           PIC X(20) VALUE PROPNAME-PGSQL-RESETCONN.
           03           PIC X(20) VALUE PROPNAME-PGSQL-EXECUTEAS.
           03           PIC X(20) VALUE PROPNAME-PGSQL-REVERT.
           03           PIC X(20) VALUE PROPNAME-PGSQL-XATRIGGER.
           03           PIC X(20) VALUE PROPNAME-PGSQL-SYNCROLLBK.
           03           PIC X(20) VALUE PROPNAME-PGSQL-RSTHSTCON.
           03           PIC X(20) VALUE PROPNAME-PGSQL-SETCONN.
           03           PIC X(20) VALUE spaces.
       01  ws-prop-name REDEFINES ws-prop-names
                        PIC X(20) OCCURS 122.
      *----------------------------------------------------------------
      *    THIS_COMPONENT events
      *----------------------------------------------------------------
       78  EVENT-MSSQL-INIT                  VALUE 1.
       78  EVENT-MSSQL-OPEN-BADOPENSTRING    VALUE 2.
       78  EVENT-MSSQL-OPEN-CONNECT          VALUE 3.
       78  EVENT-MSSQL-OPEN-GETDTC           VALUE 4.
       78  EVENT-MSSQL-XASTART               VALUE 5.
       78  EVENT-MSSQL-START-ENLIST          VALUE 6.
       78  EVENT-MSSQL-XACOMMIT              VALUE 7.
       78  EVENT-MSSQL-XAROLLBACK            VALUE 8.
       78  EVENT-MSSQL-CLOSE                 VALUE 9.
       78  EVENT-MSSQL-XAEND                 VALUE 10.
       78  EVENT-MSSQL-DEENLIST              VALUE 11.
       78  EVENT-MSSQL-XAOPEN                VALUE 12.
       78  EVENT-MSSQL-XACLOSE               VALUE 13.
       78  EVENT-MSSQL-XARECOVER             VALUE 14.
       78  EVENT-MSSQL-XAFORGET              VALUE 15.
       78  EVENT-MSSQL-XACOMPLETE            VALUE 16.
       78  EVENT-MSSQL-AXREG                 VALUE 17.
       78  EVENT-MSSQL-XAPREPARE             VALUE 18.

       78  EVENT-ODBC-START                  VALUE 21.
       78  EVENT-ODBC-COMMIT                 VALUE 22.
       78  EVENT-ODBC-ROLLBACK               VALUE 23.
       78  EVENT-ODBC-CLOSE                  VALUE 24.
       78  EVENT-ODBC-INIT                   VALUE 26.
       78  EVENT-ODBC-OPEN-BADOPENSTRING     VALUE 27.
       78  EVENT-ODBC-OPEN-CONNECT           VALUE 28.
       78  EVENT-ODBC-SYNCPOINT              VALUE 29.
       78  EVENT-ODBC-RESETCONN              VALUE 30.
       78  EVENT-ODBC-EXECUTEAS              VALUE 31.
       78  EVENT-ODBC-REVERT                 VALUE 32.
       78  EVENT-ODBC-XATRIGGER              VALUE 33.
       78  EVENT-ODBC-SYNCROLLBK             VALUE 34.
       78  EVENT-ODBC-RSETHSTCON             VALUE 35.
       78  EVENT-ODBC-SETCONN                VALUE 36.
       78  EVENT-ODBC-SETISOLEVEL            VALUE 37.

       78  EVENT-DB2-START                   VALUE 41.
       78  EVENT-DB2-END                     VALUE 42.
       78  EVENT-DB2-OPEN                    VALUE 43.
       78  EVENT-DB2-CLOSE                   VALUE 44.
       78  EVENT-DB2-COMMIT                  VALUE 45.
       78  EVENT-DB2-ROLLBACK                VALUE 46.
       78  EVENT-DB2-PREPARE                 VALUE 47.
       78  EVENT-DB2-DB2LOAD                 VALUE 48.
       78  EVENT-DB2-AXREG                   VALUE 49.
       78  EVENT-DB2-OPEN-BADOPENSTRING      VALUE 50.
       78  EVENT-DB2-SETISOLEVEL             VALUE 51.

       78  EVENT-DB2OPC-OPEN-BADOPENSTRING   VALUE 61.
       78  EVENT-DB2OPC-OPEN-CONNECT         VALUE 62.
       78  EVENT-DB2OPC-COMMIT               VALUE 63.
       78  EVENT-DB2OPC-ROLLBACK             VALUE 64.
       78  EVENT-DB2OPC-CLOSE                VALUE 65.
       78  EVENT-DB2OPC-PREOPEN              VALUE 66.
       78  EVENT-DB2OPC-PRECOMMIT            VALUE 67.
       78  EVENT-DB2OPC-PREROLLBACK          VALUE 68.
       78  EVENT-DB2OPC-PRECLOSE             VALUE 69.

       78  EVENT-ORA-START                   VALUE 81.
       78  EVENT-ORA-END                     VALUE 82.
       78  EVENT-ORA-OPEN                    VALUE 83.
       78  EVENT-ORA-CLOSE                   VALUE 84.
       78  EVENT-ORA-COMMIT                  VALUE 85.
       78  EVENT-ORA-ROLLBACK                VALUE 86.
       78  EVENT-ORA-PREPARE                 VALUE 87.
       78  EVENT-ORA-CASLOAD                 VALUE 88.
       78  EVENT-ORA-ORALOAD                 VALUE 89.
       78  EVENT-ORA-XAOSW                   VALUE 90.
       78  EVENT-ORA-RECOVER                 VALUE 91.
       78  EVENT-ORA-AXREG                   VALUE 92.
       78  EVENT-ORA-SETISOLEVEL             VALUE 93.
       78  EVENT-ORA-XAPING                  VALUE 94.

       78  EVENT-ORAOPC-OPEN-BADOPENSTRING   VALUE 101.
       78  EVENT-ORAOPC-OPEN-CONNECT         VALUE 102.
       78  EVENT-ORAOPC-COMMIT               VALUE 103.
       78  EVENT-ORAOPC-ROLLBACK             VALUE 104.
       78  EVENT-ORAOPC-CLOSE                VALUE 105.
       78  EVENT-ORAOPC-PREOPEN              VALUE 106.
       78  EVENT-ORAOPC-PRECOMMIT            VALUE 107.
       78  EVENT-ORAOPC-PREROLLBACK          VALUE 108.
       78  EVENT-ORAOPC-PRECLOSE             VALUE 109.

       78  EVENT-OCI-START                   VALUE 121.
       78  EVENT-OCI-END                     VALUE 122.
       78  EVENT-OCI-OPEN                    VALUE 123.
       78  EVENT-OCI-CLOSE                   VALUE 124.
       78  EVENT-OCI-COMMIT                  VALUE 125.
       78  EVENT-OCI-ROLLBACK                VALUE 126.
       78  EVENT-OCI-PREPARE                 VALUE 127.
       78  EVENT-OCI-CASLOAD                 VALUE 128.
       78  EVENT-OCI-OCILOAD                 VALUE 129.
       78  EVENT-OCI-XAOSW                   VALUE 130.
       78  EVENT-OCI-BIND                    VALUE 131.
       78  EVENT-OCI-UNBIND                  VALUE 132.
       78  EVENT-OCI-XATRIGGER               VALUE 133.
       78  EVENT-OCI-AXREG                   VALUE 134.
       78  EVENT-OCI-SYNCPOINT               VALUE 135.
       78  EVENT-OCI-RESETCONN               VALUE 136.
       78  EVENT-OCI-SETCONN                 VALUE 137.
       78  EVENT-OCI-CURRENTCONN             VALUE 138.
       78  EVENT-OCI-SETISOLEVEL             VALUE 139.

       78  EVENT-OCIOPC-OPEN-BADOPENSTRING   VALUE 141.
       78  EVENT-OCIOPC-OPEN-CONNECT         VALUE 142.
       78  EVENT-OCIOPC-COMMIT               VALUE 143.
       78  EVENT-OCIOPC-ROLLBACK             VALUE 144.
       78  EVENT-OCIOPC-CLOSE                VALUE 145.
       78  EVENT-OCIOPC-PREOPEN              VALUE 146.
       78  EVENT-OCIOPC-PRECOMMIT            VALUE 147.
       78  EVENT-OCIOPC-PREROLLBACK          VALUE 148.
       78  EVENT-OCIOPC-PRECLOSE             VALUE 149.

       78  EVENT-XDB-START                   VALUE 160.
       78  EVENT-XDB-END                     VALUE 161.
       78  EVENT-XDB-OPEN                    VALUE 162.
       78  EVENT-XDB-CLOSE                   VALUE 163.
       78  EVENT-XDB-COMMIT                  VALUE 164.
       78  EVENT-XDB-ROLLBACK                VALUE 165.
       78  EVENT-XDB-PREPARE                 VALUE 166.
       78  EVENT-XDB-XDBLOAD                 VALUE 167.
       78  EVENT-XDB-RESETCONN               VALUE 168.

       78  EVENT-XDBOPC-OPEN-CONNECT         VALUE 180.
       78  EVENT-XDBOPC-COMMIT               VALUE 181.
       78  EVENT-XDBOPC-ROLLBACK             VALUE 182.
       78  EVENT-XDBOPC-CLOSE                VALUE 183.
       78  EVENT-XDBOPC-PREOPEN              VALUE 184.
       78  EVENT-XDBOPC-PRECOMMIT            VALUE 185.
       78  EVENT-XDBOPC-PREROLLBACK          VALUE 186.
       78  EVENT-XDBOPC-PRECLOSE             VALUE 187.
       
       78  EVENT-PGSQL-INIT                  VALUE 201.
       78  EVENT-PGSQL-OPEN-BADOPENSTRING    VALUE 202.
       78  EVENT-PGSQL-OPEN-CONNECT          VALUE 203.
       78  EVENT-PGSQL-OPEN-GETDTC           VALUE 204.
       78  EVENT-PGSQL-XASTART               VALUE 205.
       78  EVENT-PGSQL-START-ENLIST          VALUE 206.
       78  EVENT-PGSQL-XACOMMIT              VALUE 207.
       78  EVENT-PGSQL-XAROLLBACK            VALUE 208.
       78  EVENT-PGSQL-CLOSE                 VALUE 209.
       78  EVENT-PGSQL-XAEND                 VALUE 210.
       78  EVENT-PGSQL-DEENLIST              VALUE 211.
       78  EVENT-PGSQL-XAOPEN                VALUE 212.
       78  EVENT-PGSQL-XACLOSE               VALUE 213.
       78  EVENT-PGSQL-XARECOVER             VALUE 214.
       78  EVENT-PGSQL-XAFORGET              VALUE 215.
       78  EVENT-PGSQL-XACOMPLETE            VALUE 216.
       78  EVENT-PGSQL-XAREGISTER            VALUE 217.
       78  EVENT-PGSQL-XAPREPARE             VALUE 218.

       78  EVENT-MFDBFH-ENABLED              value 220.
       78  EVENT-MFDBFH-NOT-ENABLED          value 221.
       78  EVENT-MFDBFH-NOT-LOADED           value 222.
       78  EVENT-MFDBFH-CONN-REG             value 223.
       78  EVENT-MFDBFH-CONN-REG-RESULT      value 224.
       78  EVENT-MFDBFH-CONN-DEREG           value 225.
       78  EVENT-MFDBFH-CONN-DEREG-RESULT    value 226.
       78  EVENT-MFDBFH-RESET-XA-TRIG        value 227.
       78  EVENT-MFDBFH-RESET-XA-TRIG-RESULT value 228.

      *----------------------------------------------------------------
      *    Trace flags for THIS-COMPONENT. Each bit of these flags
      *    corresponds to a property being set (non-zero value) for 
      *    THIS-COMPONENT. The actual bit set for a given property
      *    corresponds to the position-1 of the property's name in the
      *    'ws-prop-name' table above. For example, if bit 0 is set,
      *    it indicates that the 'APPLES' property was set. If the 'ALL'
      *    property is set, then all bits of 'trace_flags' will be set.
      *
      *    N.B. This is just an example of how properties and their
      *    values may be processed by a component.How properties are
      *    processed is completely transparent to CTF.
      *----------------------------------------------------------------
       78  TRACE-FLAGS-ESXA-DB2              VALUE h'00000001'.
       78  TRACE-FLAGS-ESXA-MSSQL            VALUE h'00000002'.
       78  TRACE-FLAGS-ESXA-OCI              VALUE h'00000004'.
       78  TRACE-FLAGS-ESXA-ODBC             VALUE h'00000008'.
       78  TRACE-FLAGS-ESXA-ORA              VALUE h'00000010'.
       78  TRACE-FLAGS-ESXA-PGSQL            VALUE h'00000020'.
       78  TRACE-FLAGS-ESXA-XDB              VALUE h'00000040'.
       78  TRACE-FLAGS-ESXA-XDBOPC           VALUE h'00000080'.

       01  ctf-config-data.
           03  ctf-trace-level         PIC X(4) COMP-5.
           03  ctf-trace-flags         PIC X(4) COMP-5.

      *----------------------------------------------------------------
      *    Variables required for CTF event generation
      *----------------------------------------------------------------
       01  ctf-event-data.
           03  ctf-tracer-handle       PIC X(4) COMP-5.
           03  ctf-trace-event         cblt-trc-event.
           03  ctf-trace-event-lens    PIC X(4) COMP-5
                                       OCCURS MAX-TRACE-DATA.
           03  ctf-trace-event-types   PIC X(4) COMP-5
                                       OCCURS MAX-TRACE-DATA.
           03  ctf-trace-event-ptrs    POINTER
                                       OCCURS MAX-TRACE-DATA.

       78  callbackEntryPointStat      VALUE 
                            'ctf-tracer-callback-' & '(XXXXX)' & '-s'.
       78  callbackEntryPointDyn       VALUE
                            'ctf-tracer-callback-' & '(XXXXX)' & '-d'.

       01  tracingInitialised          PIC 9 VALUE 0.

       *> Generic return code value (for tracing purposes)

       01 ctftrace-rc                  PIC S9(9) COMP-5 VALUE 0.

       LOCAL-STORAGE SECTION.

       01  ls-data.
           03  ls-api-flags            PIC X(4) COMP-5.
           03  ls-bunch-size           PIC X(4) COMP-5.
           03  ls-comp-name            PIC X(16).
           03  ls-env-value            PIC X(8).
           03  ls-install-param        cblt-trc-notif-install.
           03  ls-net-size             PIC X(4) COMP-5.
           03  ls-prop-id              PIC X(4) COMP-5.
           03  ls-prop-value           PIC X(4) COMP-5.
           03  ls-trace-data-count     PIC X(4) COMP-5.
           03  ls-trace-data-desc      PIC X(140).
           03  ls-trace-start-desc     PIC X(14).
           03  ls-trace-event          PIC X(4) COMP-5.
           03  ls-trace-level          PIC X(4) COMP-5.
           03  ls-work-var             PIC X(4) COMP-5.

       01  XACTFTraceDescription       PIC X(40).
       
       *> workspace for open string macro expansion routine
       01  idx                         PIC S9(4) COMP-5.
       01  mx                          PIC S9(4) COMP-5.
       01  mPos                        PIC S9(4) COMP-5.
       01  ws-temp-len                 PIC S9(4) COMP-5.
       01  parseErrorFlag              PIC S9(4) COMP-5.
       01  ws-temp                     PIC X(256).
       01  ws-temp-2                   PIC X(256).
       01  ws-ext-uid                  PIC X(256).
       01  ws-ext-pwd                  PIC X(256).
       01  ws-ext-usrpass              PIC X(256).

       *> workspace for hex conversion
       01 temp                         PIC X.
       01 tempX redefines temp         PIC X COMP-X.

       01 temp2X                       PIC XX COMP-X.

       01 temp0                        PIC X.
       01 tempX0 redefines temp0       PIC X COMP-X.      

       01 tempA                        PIC X.
       01 tempXA redefines tempA       PIC X COMP-X.      

       *> Host variable use when probing database to detect dead connections

       01  dummy                       pic x(29).

       *> general purpose error flag for internal routines
       *> -1 = Error, 0 = Success, 1 = Success, but with warning

       01  resultCode                  PIC s9(4) comp-5.

      $if MFDBFH-SUPPORT defined
       01  ls-cancel-proc-params       cblt-cancel-proc-params.
       01  ls-mfdbfh-pptr              procedure-pointer.
       01  ls-mfdbfh-observer-pptr     procedure-pointer.
      $end MFDBFH-SUPPORT defined

       01 ls-swtype                          PIC X COMP-X. 

       LINKAGE SECTION.

       *> Information passed in from Enterprise Server regarding
       *> the current transaction.

       01 lk-xid.
      $IF P64 SET
      $    IF XAPLATFORM = "WINDOWS"
           03 lk-xid-formatid          PIC S9(9) COMP-5.
           03 lk-xid-gtrid-length      PIC S9(9) COMP-5.
           03 lk-xid-bqual-length      PIC S9(9) COMP-5.
      $    ELSE
           03 lk-xid-formatid          PIC S9(18) COMP-5.
           03 lk-xid-gtrid-length      PIC S9(18) COMP-5.
           03 lk-xid-bqual-length      PIC S9(18) COMP-5.
      $    END-IF
      $ELSE
           03 lk-xid-formatid          PIC S9(9) COMP-5.
           03 lk-xid-gtrid-length      PIC S9(9) COMP-5.
           03 lk-xid-bqual-length      PIC S9(9) COMP-5.
      $END
           03 lk-xid-value             PIC X(128).

       01 lk-rmid                      PIC S9(9) COMP-5.
      $IF P64 SET
       01 lk-flags                     PIC S9(18) COMP-5.
      $IF XAPLATFORM = "WINDOWS"
       01 lk-count                     PIC S9(9) COMP-5.
      $ELSE
       01 lk-count                     PIC S9(18) COMP-5.
      $END
      $ELSE
       01 lk-flags                     PIC S9(9) COMP-5.
       01 lk-count                     PIC S9(9) COMP-5.
      $END
       01 lk-open-string               PIC X(256).
       01 lk-close-string              PIC X(256).
       01 lk-rc                        PIC S9(9) COMP-5.

       *> Tracing support

       01  lk-ctf-tracer-handle        PIC X(4) COMP-5.
       01  lk-ctf-notif-type           PIC X(4) COMP-5.

       01  lk-ctf-notif-param          PIC X.
       01  lk-ctf-notif-param-level    REDEFINES lk-ctf-notif-param
                                       PIC X(4) COMP-5.
       01  lk-ctf-notif-param-property REDEFINES lk-ctf-notif-param
                                       cblt-trc-notif-prop-change.
       01  lk-ctf-property-name        PIC X.
       01  lk-ctf-property-value       PIC X.

       01  lk-xaid                     PIC X(8).   

      $IF LKSWITCH = "FULL"
       01 lk-switch-area.
           03 lk-oraxa-RM-name               PIC X(32).
           03 lk-oraxa-flags                 PIC S9(9) COMP-5.
           03 lk-oraxa-version               PIC S9(9) COMP-5.
           03 lk-oraxa-open-entry            PROCEDURE-POINTER.
           03 lk-oraxa-close-entry           PROCEDURE-POINTER.
           03 lk-oraxa-start-entry           PROCEDURE-POINTER.
           03 lk-oraxa-end-entry             PROCEDURE-POINTER.
           03 lk-oraxa-rollback-entry        PROCEDURE-POINTER.
           03 lk-oraxa-prepare-entry         PROCEDURE-POINTER.
           03 lk-oraxa-commit-entry          PROCEDURE-POINTER.
           03 lk-oraxa-recover-entry         PROCEDURE-POINTER.
           03 lk-oraxa-forget-entry          PROCEDURE-POINTER.
           03 lk-oraxa-complete-entry        PROCEDURE-POINTER.
      $ELSE
       01  lk-switch-area              PIC X.
      $END

       01 lk-api-hook-params.
           03 lk-api-family            PIC X(16).
           03 lk-api-operation         PIC X(8).
           03 filler.
               05  lk-api-params        PIC X.

               05  DSNALI-params redefines lk-api-params.
                   07 DSNALI-RIBPTR            POINTER.
                   07 DSNALI-EIBPTR            POINTER.
                   07 DSNALI-DECPPTR           POINTER.
                   07 DSNALI-SQLCA             POINTER.
                   07 DSNALI-FUNCTN        PIC X(12).
                   07 DSNALI-SSNM           PIC X(4).
                   07 DSNALI-TERMECB        PIC S9(8) COMP.
                   07 DSNALI-STARTECB       PIC S9(8) COMP.
                   07 DSNALI-RETCODE        PIC S9(8) COMP.
                   07 DSNALI-REASCODE       PIC S9(8) COMP.
                   07 DSNALI-PLAN           PIC X(8).
                   07 DSNALI-TERMOP         PIC X(4).
                   07 DSNALI-SRDURA            PIC X(10).
                   07 DSNALI-GROUPOVERRIDE     PIC X(8).

               05 DSNRLI-params redefines lk-api-params.
                   07 DSNRLI-RIBPTR          POINTER.
                   07 DSNRLI-EIBPTR          POINTER.
                   07 DSNRLI-TERMECB           POINTER.
                   07 DSNRLI-STARTECB          POINTER.
                   07 DSNRLI-DECPPTR           POINTER.
                   07 DSNRLI-XID               POINTER.
                   07 DSNRLI-ACCT-STRING       POINTER.
                   07 DSNRLI-ACEE-PTR          POINTER.
                   07 DSNRLI-PKG-LST-PTR       POINTER.
                   07 DSNRLI-FUNCTION          PIC X(18).
                   07 DSNRLI-SUBSYSTEM         PIC X(4).
                   07 DSNRLI-RETURN-CODE       PIC S9(8) BINARY.
                   07 DSNRLI-REASON-CODE     PIC X(4).
                   07 DSNRLI-CORRELID        PIC X(12).
                   07 DSNRLI-ACCT-TOKEN      PIC X(22).
                   07 DSNRLI-ACCT-INTERVAL   PIC X(6).
                   07 DSNRLI-PLANNAME        PIC X(8).
                   07 DSNRLI-COLLECTION      PIC X(18).
                   07 DSNRLI-REUSE           PIC X(8).
                   07 DSNRLI-GROUPOVERRIDE     PIC X(8).
                   07 DSNRLI-USER              PIC X(16).
                   07 DSNRLI-APPL              PIC X(32).
                   07 DSNRLI-WS                PIC X(18).
                   07 DSNRLI-PRIME-AUTH        PIC X(8).
                   07 DSNRLI-SECND-AUTH        PIC X(8).
                   07 DSNRLI-CNTX-KEY          PIC X(32).
                   07 DSNRLI-PROG-ID           PIC X(80).
       
       01 lk-TMFLags                         PIC S9(8) COMP-5. 

       01 lk-ServerName                      PIC X(256).

          *> lk-swtype: returns the DBMS type of switch
          *>            1 - DB2
          *>            2 - MQ
          *>            3 - MSSQL
          *>            4 - OCI
          *>            5 - ODBC
          *>            6 - ORACLE
          *>            7 - PostgreSQL
          *>            8 - XDB
       01 lk-swtype                          PIC X COMP-X. 

