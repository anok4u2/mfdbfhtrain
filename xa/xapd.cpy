      *****************************************************************
      *    Set appropriate trace flags bit
      *****************************************************************
       apply-(XXXXX)-property-value section.

           IF ls-prop-id = PROPID-ALL
               IF ls-prop-value NOT = 0
                  COMPUTE ctf-trace-flags = ctf-trace-flags b-or
                                            h'FFFFFFFF'
               ELSE
                  MOVE 0 TO ctf-trace-flags
               END-IF
           ELSE
               COMPUTE ls-work-var = 2 ** (ls-prop-id - 2)

               IF ls-prop-value NOT = 0
                   COMPUTE ctf-trace-flags = ctf-trace-flags b-or
                                             ls-work-var
               ELSE
                   COMPUTE ctf-trace-flags = ctf-trace-flags b-and b-not
                                             ls-work-var
               END-IF
           END-IF
           .

      *****************************************************************
      *  Initialise tracing for THIS-COMPONENT.
      *
      *  On exit from this section 'ctf-tracer-handle',
      *  'ctf-trace-flags' and 'ctf-trace-level' will be set for
      *  THIS-COMPONENT
      *****************************************************************
       initialise-(XXXXX)-tracing section.

           MOVE 0 TO ctf-trace-flags

           *>
           *>  Pick up THIS-COMPONENT's tracer handle
           *>
           MOVE 78-CTF-FLAG-COMP-NAME-NULL-TERM TO ls-api-flags
           MOVE THIS-COMPONENT & x"00" TO ls-comp-name

           CALL "CBL_CTF_TRACER_GET" USING
                                     BY VALUE     ls-api-flags
                                     BY REFERENCE ls-comp-name
                                     BY REFERENCE ctf-tracer-handle
           END-CALL

           IF RETURN-CODE NOT = 0
               DISPLAY "CBL_CTF_TRACER_GET (rc = "
                       RETURN-CODE
                       ")"
               EXIT SECTION
           END-IF

           MOVE 78-CTF-FLAG-PROP-INT-VALUE TO ls-api-flags

           PERFORM VARYING ls-prop-id FROM 1 BY 1
                     UNTIL ws-prop-name(ls-prop-id) = SPACES

               CALL "CBL_CTF_COMP_PROPERTY_GET" USING
                              BY VALUE     ls-api-flags
                              BY REFERENCE ctf-tracer-handle
                              BY REFERENCE ws-prop-name(ls-prop-id)
                              BY VALUE     0
                              BY REFERENCE ls-prop-value
               END-CALL

               IF RETURN-CODE = 0
                   PERFORM apply-(XXXXX)-property-value
               END-IF
           END-PERFORM

           *>
           *>  Get THIS-COMPONENT's tracer trace level
           *>
           MOVE 0 TO ls-api-flags

           CALL "CBL_CTF_TRACER_LEVEL_GET" USING
                                       BY VALUE     ls-api-flags
                                       BY REFERENCE ctf-tracer-handle
                                       BY REFERENCE ctf-trace-level
           END-CALL

           IF RETURN-CODE NOT = 0
               DISPLAY "CBL_TRACER_LEVEL_GET (rc = "
                       return-code
                       ")"
               EXIT SECTION
           END-IF

           *>
           *> Install tracer notification callback
           *>
           MOVE LOW-VALUES TO ls-install-param
           MOVE ctf-tracer-handle TO
                cblte-tni-handle OF ls-install-param
           SET cblte-tni-callback OF ls-install-param TO
      $IF DYNAMICREGISTRATION = "Y"
               ENTRY callbackEntryPointDyn
      $ELSE
               ENTRY callbackEntryPointStat
      $END
           CALL "CBL_CTF_TRACER_NOTIFY" USING
                                        BY VALUE 0
                                        BY REFERENCE ls-install-param
           END-CALL

           IF RETURN-CODE NOT = 0
               DISPLAY "CBL_CTF_TRACER_NOTIFY (rc = "
                       return-code
                       ")"
               EXIT SECTION
           END-IF

           *>
           *> Initialise fixed fields in trace event structure
           *>
           MOVE LOW-VALUES TO ctf-trace-event

           SET cblte-trcevt-event-len OF ctf-trace-event TO
               ADDRESS OF ctf-trace-event-lens(1)
           SET cblte-trcevt-event-type OF ctf-trace-event TO
               ADDRESS OF ctf-trace-event-types(1)
           SET cblte-trcevt-event-data OF ctf-trace-event TO
               ADDRESS OF ctf-trace-event-ptrs(1)
           .


      *****************************************************************
      *  THIS-COMPONENT tracer callback. Processes THIS-COMPONENT's
      *  property and trace level changes.
      *
      *  lk-ctf-tracer-handle  =  tracer handle
      *
      *  lk-ctf-notif-type     =  notification type
      *
      *                           0 = property change
      *                           1 = trace level change
      *
      *  lk-ctf-notif-info     =  address of item whose type is
      *                           dependent upon notification type
      *
      *                           type 0 = cblt-trc-notif-prop-change
      *                           type 1 = pic x(4) comp-5
      *****************************************************************
       ctf-tracer-(XXXXX)-notif-callback section.
      $IF DYNAMICREGISTRATION = "Y"
               ENTRY callbackEntryPointDyn
      $ELSE
               ENTRY callbackEntryPointStat
      $END
               USING
                     BY VALUE     lk-ctf-tracer-handle
                     BY VALUE     lk-ctf-notif-type
                     BY REFERENCE lk-ctf-notif-param.
           EVALUATE lk-ctf-notif-type
               WHEN 78-TRC-NOTIF-TYPE-PROP-CHANGE
                   PERFORM ctf-tracer-(XXXXX)-notif-property

               WHEN 78-TRC-NOTIF-TYPE-LEVEL-CHANGE
                   MOVE lk-ctf-notif-param-level TO ctf-trace-level

               WHEN OTHER
                    CONTINUE
           END-EVALUATE

           GOBACK
           .

      *-----------------------------------------------------------------
      *    Tracer property changed
      *-----------------------------------------------------------------
       ctf-tracer-(XXXXX)-notif-property section.
           SET ADDRESS OF lk-ctf-property-name TO
               cblte-tnpc-name OF lk-ctf-notif-param-property

           PERFORM VARYING ls-prop-id FROM 1 BY 1
                     UNTIL ws-prop-name(ls-prop-id) = SPACES

               IF ws-prop-name(ls-prop-id) =
                  lk-ctf-property-name
                   (1:cblte-tnpc-namelen OF lk-ctf-notif-param-property)

                   MOVE cblte-tnpc-valint OF lk-ctf-notif-param-property
                        TO ls-prop-value

                   PERFORM apply-(XXXXX)-property-value
                   EXIT PERFORM
               END-IF
           END-PERFORM
           .

      *****************************************************************
      *    Output trace event
      *****************************************************************
       trace-(XXXXX)-event section.
      $IF XABUILD = "DB2"
           IF (TRACE-FLAGS-ESXA-DB2 b-and ctf-trace-flags) = 0
              EXIT SECTION
           END-IF
      $END
      $IF XABUILD = "SQLSRVR"
           IF (TRACE-FLAGS-ESXA-MSSQL b-and ctf-trace-flags) = 0
              EXIT SECTION
           END-IF
      $END
      $IF XABUILD = "OCI"
           IF (TRACE-FLAGS-ESXA-OCI b-and ctf-trace-flags) = 0
              EXIT SECTION
           END-IF
      $END
      $IF XABUILD = "ODBC"
           IF (TRACE-FLAGS-ESXA-ODBC b-and ctf-trace-flags) = 0
              EXIT SECTION
           END-IF
      $END
      $IF XABUILD = "ORA"
           IF (TRACE-FLAGS-ESXA-ORA b-and ctf-trace-flags) = 0
              EXIT SECTION
           END-IF
      $END
      $IF XABUILD = "PGSQL"
           IF (TRACE-FLAGS-ESXA-PGSQL b-and ctf-trace-flags) = 0
              EXIT SECTION
           END-IF
      $END
      $IF XABUILD = "XDB"
           IF (TRACE-FLAGS-ESXA-XDB b-and ctf-trace-flags) = 0
              EXIT SECTION
           END-IF
      $END
      $IF XABUILD = "XDBOPC"
           IF (TRACE-FLAGS-ESXA-XDBOPC b-and ctf-trace-flags) = 0
              EXIT SECTION
           END-IF
      $END
           MOVE 78-TRACE-EVENT-FLAGS-NONE TO
                cblte-trcevt-flags OF ctf-trace-event
           MOVE ls-trace-event TO
                cblte-trcevt-event-id OF ctf-trace-event
           MOVE ls-trace-level TO
                cblte-trcevt-level OF ctf-trace-event

           MOVE 0 TO ls-trace-data-count

           EVALUATE ls-trace-event

      $IF XABUILD = "SQLSRVR"
               *> SQL Server XA
               WHEN EVENT-MSSQL-OPEN-CONNECT
               WHEN EVENT-MSSQL-START-ENLIST
               WHEN EVENT-MSSQL-DEENLIST
               WHEN EVENT-MSSQL-CLOSE
               WHEN EVENT-ODBC-CLOSE
               WHEN EVENT-ODBC-COMMIT
               WHEN EVENT-ODBC-EXECUTEAS
               WHEN EVENT-ODBC-REVERT
               WHEN EVENT-ODBC-ROLLBACK
               WHEN EVENT-ODBC-RESETCONN
               WHEN EVENT-ODBC-RSETHSTCON
               WHEN EVENT-ODBC-SETCONN
               WHEN EVENT-ODBC-SETISOLEVEL
               WHEN EVENT-ODBC-SYNCPOINT
               WHEN EVENT-ODBC-SYNCROLLBK
               WHEN EVENT-ODBC-XATRIGGER
                   PERFORM trc-mssql-event-rtn-sqlerr

               WHEN EVENT-MSSQL-OPEN-GETDTC
                   PERFORM trc-mssql-event-rtncode

               WHEN EVENT-MSSQL-OPEN-BADOPENSTRING
                   PERFORM trc-mssql-event-openstr-err

               WHEN EVENT-MSSQL-INIT
                   PERFORM trc-odbc-event-info

               WHEN  EVENT-MSSQL-XAOPEN
               WHEN  EVENT-MSSQL-XACLOSE
               WHEN  EVENT-MSSQL-XARECOVER
               WHEN  EVENT-MSSQL-XAFORGET
               WHEN  EVENT-MSSQL-XACOMPLETE
               WHEN  EVENT-MSSQL-AXREG     
               WHEN  EVENT-MSSQL-XAPREPARE
               WHEN  EVENT-MSSQL-XASTART
               WHEN  EVENT-MSSQL-XAEND
               WHEN  EVENT-MSSQL-XACOMMIT
               WHEN  EVENT-MSSQL-XAROLLBACK
               WHEN  EVENT-MSSQL-XACLOSE
                   PERFORM trc-mssql-event-xacall
      $END

      $IF XABUILD = "PGSQL"
               *> SQL Server XA
               WHEN EVENT-PGSQL-OPEN-CONNECT
               WHEN EVENT-PGSQL-START-ENLIST
               WHEN EVENT-PGSQL-DEENLIST
               WHEN EVENT-PGSQL-CLOSE
               WHEN EVENT-ODBC-CLOSE
               WHEN EVENT-ODBC-COMMIT
               WHEN EVENT-ODBC-EXECUTEAS
               WHEN EVENT-ODBC-REVERT
               WHEN EVENT-ODBC-ROLLBACK
               WHEN EVENT-ODBC-RESETCONN
               WHEN EVENT-ODBC-RSETHSTCON
               WHEN EVENT-ODBC-SETCONN
               WHEN EVENT-ODBC-SETISOLEVEL
               WHEN EVENT-ODBC-SYNCPOINT
               WHEN EVENT-ODBC-SYNCROLLBK
               WHEN EVENT-ODBC-XATRIGGER
                   PERFORM trc-pgsql-event-rtn-sqlerr

               WHEN EVENT-PGSQL-OPEN-GETDTC
                   PERFORM trc-pgsql-event-rtncode

               WHEN EVENT-PGSQL-OPEN-BADOPENSTRING
                   PERFORM trc-pgsql-event-openstr-err

               WHEN EVENT-PGSQL-INIT
                   PERFORM trc-odbc-event-info

               WHEN  EVENT-PGSQL-XAOPEN
               WHEN  EVENT-PGSQL-XACLOSE
               WHEN  EVENT-PGSQL-XARECOVER
               WHEN  EVENT-PGSQL-XAFORGET
               WHEN  EVENT-PGSQL-XACOMPLETE
               WHEN  EVENT-PGSQL-XAREGISTER
               WHEN  EVENT-PGSQL-XAPREPARE
               WHEN  EVENT-PGSQL-XASTART
               WHEN  EVENT-PGSQL-XAEND
               WHEN  EVENT-PGSQL-XACOMMIT
               WHEN  EVENT-PGSQL-XAROLLBACK
               WHEN  EVENT-PGSQL-XACLOSE
                   PERFORM trc-pgsql-event-xacall
      $END

      $IF XABUILD = "ODBC"
               *> ODBC 1 phase commit

               WHEN EVENT-ODBC-OPEN-CONNECT
               WHEN EVENT-ODBC-START
               WHEN EVENT-ODBC-COMMIT
               WHEN EVENT-ODBC-ROLLBACK
               WHEN EVENT-ODBC-CLOSE
               WHEN EVENT-ODBC-SYNCPOINT
               WHEN EVENT-ODBC-RESETCONN
               WHEN EVENT-ODBC-EXECUTEAS
               WHEN EVENT-ODBC-REVERT
               WHEN EVENT-ODBC-SYNCROLLBK
               WHEN EVENT-ODBC-RSETHSTCON
               WHEN EVENT-ODBC-SETCONN
               WHEN EVENT-ODBC-SETISOLEVEL
               WHEN EVENT-ODBC-SYNCPOINT
               WHEN EVENT-ODBC-XATRIGGER
                   PERFORM trc-odbc-event-rtn-sqlerr

               WHEN EVENT-ODBC-OPEN-BADOPENSTRING
                   PERFORM trc-odbc-event-openstr-err

               WHEN EVENT-ODBC-INIT
                   PERFORM trc-odbc-event-info

               WHEN  EVENT-MSSQL-AXREG
                   PERFORM trc-odbc-event-xacall
      $END

      $IF XABUILD = "DB2"
               WHEN EVENT-DB2-DB2LOAD
                   PERFORM trace-db2-event-db2load
               WHEN EVENT-DB2-START
               WHEN EVENT-DB2-END
               WHEN EVENT-DB2-COMMIT
               WHEN EVENT-DB2-ROLLBACK
               WHEN EVENT-DB2-OPEN
               WHEN EVENT-DB2-CLOSE
               WHEN EVENT-DB2-PREPARE
               WHEN EVENT-DB2-AXREG
                   PERFORM trc-db2-event-xacall

               WHEN EVENT-DB2OPC-OPEN-CONNECT
               WHEN EVENT-DB2OPC-COMMIT
               WHEN EVENT-DB2OPC-ROLLBACK
               WHEN EVENT-DB2OPC-CLOSE
                   PERFORM trc-db2opc-event-rtn-sqlerr

               WHEN EVENT-DB2-OPEN-BADOPENSTRING
               WHEN EVENT-DB2OPC-OPEN-BADOPENSTRING
                   PERFORM trc-db2-event-openstr-err

               WHEN EVENT-DB2OPC-PREOPEN
               WHEN EVENT-DB2OPC-PRECOMMIT
               WHEN EVENT-DB2OPC-PREROLLBACK
               WHEN EVENT-DB2OPC-PRECLOSE
               WHEN EVENT-DB2-SETISOLEVEL
                   PERFORM trc-db2-event-info
      $END

      $IF XABUILD = "OCI"
               WHEN EVENT-OCI-CASLOAD
                   PERFORM trc-oci-event-casload
      $IF XAPLATFORM = "WINDOWS"
               WHEN EVENT-OCI-OCILOAD
                   PERFORM trc-oci-event-ociload
      $END
               WHEN EVENT-OCI-XAOSW
                   PERFORM trc-oci-event-xaosw
               WHEN EVENT-OCI-START
               WHEN EVENT-OCI-END
               WHEN EVENT-OCI-COMMIT
               WHEN EVENT-OCI-ROLLBACK
               WHEN EVENT-OCI-OPEN
               WHEN EVENT-OCI-CLOSE
               WHEN EVENT-OCI-PREPARE
               WHEN EVENT-OCI-BIND
               WHEN EVENT-OCI-UNBIND
               WHEN EVENT-OCI-AXREG     
                   PERFORM trc-oci-event-xacall

               WHEN EVENT-OCIOPC-PREOPEN
               WHEN EVENT-OCIOPC-PRECOMMIT
               WHEN EVENT-OCIOPC-PREROLLBACK
               WHEN EVENT-OCIOPC-PRECLOSE
                   PERFORM trc-oci-event-info

               WHEN EVENT-OCIOPC-OPEN-CONNECT
               WHEN EVENT-OCIOPC-COMMIT
               WHEN EVENT-OCIOPC-ROLLBACK
               WHEN EVENT-OCIOPC-CLOSE
               WHEN EVENT-OCI-XATRIGGER
               WHEN EVENT-OCI-SYNCPOINT
               WHEN EVENT-OCI-RESETCONN
               WHEN EVENT-OCI-SETCONN
               WHEN EVENT-OCI-CURRENTCONN
                   PERFORM trc-oci-event-rtn-sqlerr

               WHEN EVENT-OCIOPC-OPEN-BADOPENSTRING
                   PERFORM trc-oci-event-openstr-err
      $END

      $IF XABUILD = "ORA"
               WHEN EVENT-ORA-CASLOAD
                   PERFORM trc-ora-event-casload
               WHEN EVENT-ORA-ORALOAD
                   PERFORM trc-ora-event-oraload
               WHEN EVENT-ORA-XAOSW
                   PERFORM trc-ora-event-xaosw
               WHEN EVENT-ORA-START
               WHEN EVENT-ORA-END
               WHEN EVENT-ORA-COMMIT
               WHEN EVENT-ORA-ROLLBACK
               WHEN EVENT-ORA-OPEN
               WHEN EVENT-ORA-CLOSE
               WHEN EVENT-ORA-PREPARE
               WHEN EVENT-ORA-RECOVER
                   PERFORM trc-ora-event-xacall

               WHEN EVENT-ORAOPC-OPEN-CONNECT
               WHEN EVENT-ORAOPC-COMMIT
               WHEN EVENT-ORAOPC-ROLLBACK
               WHEN EVENT-ORAOPC-CLOSE
               WHEN EVENT-ORA-SETISOLEVEL
                   PERFORM trc-ora-event-rtn-sqlerr

               WHEN EVENT-ORAOPC-OPEN-BADOPENSTRING
                   PERFORM trc-ora-event-openstr-err

               WHEN EVENT-ORAOPC-PREOPEN
               WHEN EVENT-ORAOPC-PRECOMMIT
               WHEN EVENT-ORAOPC-PREROLLBACK
               WHEN EVENT-ORAOPC-PRECLOSE
                   PERFORM trc-ora-event-info
      $END

      $IF XABUILD = "XDB"
               WHEN EVENT-XDB-XDBLOAD
                   PERFORM trace-xdb-event-xdbload
               WHEN EVENT-XDB-RESETCONN
                   PERFORM trace-xdb-event-resetconn
               WHEN EVENT-XDB-START
               WHEN EVENT-XDB-END
               WHEN EVENT-XDB-COMMIT
               WHEN EVENT-XDB-ROLLBACK
               WHEN EVENT-XDB-OPEN
               WHEN EVENT-XDB-CLOSE
               WHEN EVENT-XDB-PREPARE
                   PERFORM trc-xdb-event-xacall
      $END

      $IF XABUILD = "XDBOPC"
               WHEN EVENT-XDBOPC-OPEN-CONNECT
               WHEN EVENT-XDBOPC-COMMIT
               WHEN EVENT-XDBOPC-ROLLBACK
               WHEN EVENT-XDBOPC-CLOSE
                   PERFORM trc-xdbopc-event-rtn-sqlerr

               WHEN EVENT-XDBOPC-PREOPEN
               WHEN EVENT-XDBOPC-PRECOMMIT
               WHEN EVENT-XDBOPC-PREROLLBACK
               WHEN EVENT-XDBOPC-PRECLOSE
                   PERFORM trc-xdbopc-event-info
      $END

      $if MFDBFH-SUPPORT defined
               WHEN EVENT-MFDBFH-ENABLED
               WHEN EVENT-MFDBFH-NOT-ENABLED
               WHEN EVENT-MFDBFH-NOT-LOADED
               WHEN EVENT-MFDBFH-CONN-REG
               WHEN EVENT-MFDBFH-CONN-REG-RESULT
               WHEN EVENT-MFDBFH-CONN-DEREG
               WHEN EVENT-MFDBFH-CONN-DEREG-RESULT
                   perform trc-mfdbfh-event-info
      $end MFDBFH-SUPPORT defined

           END-EVALUATE

           MOVE ls-trace-data-count TO
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

       trc-odbc-event-info section.

           MOVE "ODBC Initialization complete" TO ls-trace-data-desc
           
           SET ctf-trace-event-ptrs(1) TO 
               ADDRESS OF ls-trace-data-desc

           UNSTRING ls-trace-data-desc DELIMITED BY "  " INTO TmpString
                COUNT ctf-trace-event-lens(1)

           MOVE 78-TRACE-EVENT-TYPE-TEXT TO
                ctf-trace-event-types(1)

           ADD 1 TO ls-trace-data-count
           .

      $if MFDBFH-SUPPORT defined
       trc-mfdbfh-event-info section.
           evaluate ls-trace-event
               when event-mfdbfh-conn-reg
                string
                    ">Register connection: "       delimited by size
                    ConnectionName(ConnIX)         delimited space
                    into ls-trace-data-desc
                end-string

               when event-mfdbfh-conn-reg-result
                move ws-mfdbfh-rc to ws-mfdbfh-rc-display

                string
                    "<Register connection: RC="    delimited by size
                    ws-mfdbfh-rc-display           delimited size
                    into ls-trace-data-desc
                end-string

               when event-mfdbfh-conn-dereg
                string
                    ">De-register connection: "    delimited by size
                    ConnectionName(ConnIX)         delimited space
                    into ls-trace-data-desc
                end-string

               when event-mfdbfh-conn-dereg-result
                move ws-mfdbfh-rc to ws-mfdbfh-rc-display

                string
                    "<De-register connection: RC=" delimited by size
                    ws-mfdbfh-rc-display           delimited size
                    into ls-trace-data-desc
                end-string

               when event-mfdbfh-enabled
                   move "ES database file handling enabled" TO ls-trace-data-desc

               when event-mfdbfh-not-enabled
                   move "ES database file handling not enabled" TO ls-trace-data-desc

               when event-mfdbfh-reset-xa-trig
                   string
                       ">Reset XA trigger: "       delimited by size
                       ConnectionName(ConnIX)      delimited space
                       into ls-trace-data-desc
                   end-string

               when event-mfdbfh-reset-xa-trig-result
                   move ws-mfdbfh-rc to ws-mfdbfh-rc-display

                   string
                       "<Reset XA trigger: RC="    delimited by size
                       ws-mfdbfh-rc-display        delimited size
                       into ls-trace-data-desc
                   end-string

               when event-mfdbfh-not-loaded
                   move "MFDBFH not loaded" TO ls-trace-data-desc
           end-evaluate

           set ctf-trace-event-ptrs(1) to address of ls-trace-data-desc
           unstring ls-trace-data-desc delimited by "  " into tmpstring count ctf-trace-event-lens(1)
           move 78-trace-event-type-text TO ctf-trace-event-types(1)
           add 1 to ls-trace-data-count
           exit section
           .

       mfdbfh-connection-deregister section.
           if  ws-mfdbfh-connection-dereg-pptr not = null
           and MfdbfhRegistered(ConnIX) not = 0
               if 78-ctf-flag-level-info >= ctf-trace-level
                   move event-mfdbfh-conn-dereg to ls-trace-event
                   move 78-ctf-flag-level-info to ls-trace-level
                   perform trace-(XXXXX)-event
               end-if

               call ws-mfdbfh-connection-dereg-pptr using
      $if XABUILD = "DB2"
                                                          reference hdbc(ConnIX)
      $else
                                                          reference ConnectionHandle(ConnIX)
      $end XABUILD
                                                          value     78-MFDBFH-CONN-REG-TYPE-ODBC
                                                          returning ws-mfdbfh-rc
               end-call

               if ws-mfdbfh-rc = 78-mfdbfh-conn-reg-rc-success
                   move 0 to MfdbfhRegistered(ConnIX)
               end-if

               if 78-ctf-flag-level-info >= ctf-trace-level
                   move event-mfdbfh-conn-dereg-result to ls-trace-event
                   move 78-ctf-flag-level-info to ls-trace-level
                   perform trace-(XXXXX)-event
               end-if
           end-if

           exit section
           .

       mfdbfh-connection-register section.
           if ws-mfdbfh-connection-reg-pptr not = null
               if 78-ctf-flag-level-info >= ctf-trace-level
                   move event-mfdbfh-conn-reg to ls-trace-event
                   move 78-ctf-flag-level-info to ls-trace-level
                   perform trace-(XXXXX)-event
               end-if

      $if XABUILD = "DB2"
               set ls-mfdbfh-pptr to null
      $else
      $if DYNAMICREGISTRATION = "Y"
               set ls-mfdbfh-pptr to entry esxa-register
      $else
               set ls-mfdbfh-pptr to null
      $end DYNAMICREGISTRATION = "Y"
      $end XABUILD

      $if XABUILD = "DB2"
               if ws-mfdbfh-connection-reg-cred-pptr not = null
                   call ws-mfdbfh-connection-reg-cred-pptr using reference ResName(ConnIX)
                                                                 reference hdbc(ConnIX)
                                                                 value     78-MFDBFH-CONN-REG-TYPE-ODBC
                                                                 value     ws-mfdbfh-connection-reg-flags
                                                                 reference DbString
                                                                 reference UidString
                                                                 reference PwdString
                                                                 value     ls-mfdbfh-pptr
                                                                 returning ws-mfdbfh-rc
                   end-call
               else
      $end XABUILD
               call ws-mfdbfh-connection-reg-pptr using reference ResName(ConnIX)
      $if XABUILD = "DB2"
                                                        reference hdbc(ConnIX)
      $else
                                                        reference ConnectionHandle(ConnIX)
      $end XABUILD
                                                        value     78-MFDBFH-CONN-REG-TYPE-ODBC
                                                        value     ws-mfdbfh-connection-reg-flags
                                                        value     ls-mfdbfh-pptr
                                                        returning ws-mfdbfh-rc
               end-call
      $if XABUILD = "DB2"
               end-if
      $end XABUILD

               if ws-mfdbfh-rc = 78-mfdbfh-conn-reg-rc-success
                   move 1 to MfdbfhRegistered(ConnIX)
               end-if

               if 78-ctf-flag-level-info >= ctf-trace-level
                   move event-mfdbfh-conn-reg-result to ls-trace-event
                   move 78-ctf-flag-level-info to ls-trace-level
                   perform trace-(XXXXX)-event
               end-if
           end-if

           exit section
           .

       mfdbfh-initialise section.
           *>
           *>  Determine whether ES database file handling is enabled
           *>
           move spaces to ls-env-value
           display 'ES_DB_FH' upon environment-name
           accept ls-env-value from environment-value

           call 'CBL_TOLOWER' using reference ls-env-value
                                    value     length of ls-env-value
           end-call

           if ls-env-value = 'y'
           or ls-env-value = 'yes'
           or ls-env-value = 'true'
               if 78-ctf-flag-level-info >= ctf-trace-level
                   move event-mfdbfh-enabled to ls-trace-event
                   move 78-ctf-flag-level-info to ls-trace-level
                   perform trace-(XXXXX)-event
               end-if

               set ws-mfdbfh-pptr to entry 'MFDBFH'

               if ws-mfdbfh-pptr not = null
                   set ws-mfdbfh-connection-dereg-pptr to entry 'MFDBFH_XA_CONNECTION_DEREGISTER'
                   set ws-mfdbfh-connection-reg-pptr to entry 'MFDBFH_XA_CONNECTION_REGISTER'
                   set ws-mfdbfh-connection-reg-cred-pptr to entry 'MFDBFH_XA_CONNECTION_REGISTER_CREDENTIALS'
               else
                   if 78-ctf-flag-level-warn >= ctf-trace-level
                       move event-mfdbfh-not-loaded to ls-trace-event
                       move 78-ctf-flag-level-info to ls-trace-level
                       perform trace-(XXXXX)-event
                   end-if
               end-if
           else
               if 78-ctf-flag-level-info >= ctf-trace-level
                   move event-mfdbfh-not-enabled to ls-trace-event
                   move 78-ctf-flag-level-info to ls-trace-level
                   perform trace-(XXXXX)-event
               end-if
           end-if

           exit section
           .

       mfdbfh-reset-xa-trigger section.
      $if DYNAMICREGISTRATION = "Y"
           if MfdbfhRegistered(ConnIX) not = 0
           if ws-mfdbfh-connection-reg-pptr not = null
               if 78-ctf-flag-level-info >= ctf-trace-level
                   move event-mfdbfh-reset-xa-trig to ls-trace-event
                   move 78-ctf-flag-level-info to ls-trace-level
                   perform trace-(XXXXX)-event
               end-if

      $if XABUILD = "DB2"
               set ls-mfdbfh-pptr to null
      $else
               set ls-mfdbfh-pptr to hvppTrigger
      $end XABUILD

               call ws-mfdbfh-connection-reg-pptr using reference ResName(ConnIX)
      $if XABUILD = "DB2"
                                                        reference hdbc(ConnIX)
      $else
                                                        reference ConnectionHandle(ConnIX)
      $end XABUILD
                                                        value     78-MFDBFH-CONN-REG-TYPE-ODBC
                                                        value     ws-mfdbfh-connection-reg-flags
                                                        value     ls-mfdbfh-pptr
                                                        returning ws-mfdbfh-rc
               end-call

               if 78-ctf-flag-level-info >= ctf-trace-level
                   move event-mfdbfh-reset-xa-trig-result to ls-trace-event
                   move 78-ctf-flag-level-info to ls-trace-level
                   perform trace-(XXXXX)-event
               end-if
           end-if

      $end DYNAMICREGISTRATION = "Y"
       exit section
       .

      $end MFDBFH-SUPPORT defined


       GetCustomizationParams SECTION. 
           move 0 to returnCode
           move 0 to reasonCode
           move lk-open-string to xaOpenString
           initialize password
           move 7 to entriesUsed

           move "JOBTYPE" to vName(1)
           evaluate XAi-SEP-type
               when sepTypeUnknown             
                   move "UNKNOWN" to vValue(1)
               when  sepIsCICS                   
                   move "CICS" to vValue(1)
               when  sepIsIMS                   
                   move "IMS" to vValue(1)
               when  sepIsJCL                  
                   move "JCL" to vValue(1)
               when  sepIsWebServices            
                   move "WEBSERVICES" to vValue(1)
               when  sepIsAdmin                 
                   move "ADMIN" to vValue(1)
               when  sepIsOther
                   move "OTHER" to vValue(1)
           end-evaluate
           
           move "MODULE" to vName(2)
           move MODULENAME to vValue(2)

           if XAi-computer-name-ptr not = null
      $IF P64 SET
                   AND XAi-computer-name-ptr-X not = X'20202020202020'
      $ELSE
                   AND XAi-computer-name-ptr-X not = X'20202020'
      $END
               move "SERVER" to vName(3)
               move spaces to vValue(3)
               set address of lk-ServerName to XAi-computer-name-ptr
               string
                 lk-ServerName delimited space
               into vValue(3)
           else
               MOVE "UNKNOWN" TO vValue(3)
           end-if

           move "REGION" to vName(4)
           move XAi-Region-Name to vValue(4)

           move "XARN" to vName(5)
           move XAi-XA-Resource-Name to vValue(5)

           move "INITUSER" to vName(6)
           move XAi-SEP-initial-user to vValue(6)

           move "CURRUSER" to vName(7)
           move XAi-current-user to vValue(7)

           IF CustomizationExitRoutine not = null
               call "ESXAEXTCFG" using by reference xaCustomizationInfo
               if returnCode not = 0
                   move returnCode to ws-returnCode
                   move reasonCode to ws-reasonCode
                   MOVE 1 TO MsgLen
                   MOVE 1 TO MsgLevel
                   MOVE 1 TO EsLoggingLevel(ConnIX)
                   STRING
                       XAi-XA-Resource-Name DELIMITED SPACE
                       ": Customization Error." DELIMITED SIZE
                       " ReturnCode = " DELIMITED SIZE
                       ws-returnCode DELIMITED SIZE
                       " ReasonCode = " DELIMITED SIZE
                       ws-reasonCode DELIMITED SIZE
                   INTO consoleMessage pointer MsgLen
                              SUBTRACT 1 FROM MsgLen
                   CALL "mfxaLogConsoleMsg" USING 
                              consoleMessage MsgLen 
                              EsLoggingLevel(ConnIX) MsgLevel  
                   END-CALL
                   move -5 to ws-open-rc
                   GOBACK RETURNING ws-open-rc 
               end-if

               move xaOpenString to ws-open-string
           END-IF

           EXIT.       

       ExpandOpenStringMacros SECTION.
           MOVE '&' TO MacroDelim
           PERFORM VaultExpandMacros
           exit.
       
       ExpandPackagePathMacros SECTION.
           MOVE 0 TO parseErrorFlag
           MOVE 1 TO idx
           PERFORM UNTIL idx > 256
                   OR ws-open-STRING(idx:1) = LOW-VALUE
               IF ws-open-STRING(idx:1) = '%'
                   MOVE idx TO mPos
                   ADD 1 TO idx
                   IF idx > 256 OR ws-open-STRING(idx:1) = LOW-VALUES
                       MOVE 1 TO parseErrorFlag
                       EXIT SECTION
                   END-IF
                   IF ws-open-STRING(idx:1) = '%'
                       *> %% expands to a single %
                       MOVE ws-open-STRING(idx:) TO ws-temp
                       MOVE ws-temp TO ws-open-STRING(mPos:)
                       ADD 1 TO idx
                       EXIT PERFORM CYCLE
                   END-IF
                   MOVE 1 TO mx
                   INITIALIZE ws-temp
                   PERFORM UNTIL EXIT
                       MOVE ws-open-STRING(idx:) TO ws-temp(mx:1)
                       ADD 1 TO mx
                       ADD 1 TO idx
                       IF idx > 256
                               OR ws-open-STRING(idx:1) = LOW-VALUES
                           MOVE 1 TO parseErrorFlag
                           EXIT SECTION
                       END-IF
                       IF ws-open-STRING(idx:1) = '%'
                           EXIT PERFORM
                       END-IF
                   END-PERFORM
                   CALL "CBL_TOUPPER" USING
                       ws-temp
                   BY VALUE mx
                   EVALUATE ws-temp
                   WHEN "REGION"
                       MOVE XAi-Region-Name TO ws-temp-2
                   WHEN "XARN"
                       MOVE ResName(ConnIX) TO ws-temp-2
                   WHEN "ESUSER"
                       IF XAi-current-user = spaces 
                       OR XAi-current-user(1:1) = LOW-VALUE
                       OR XAi-current-user = "CICSUSER" 
                       OR XAi-current-user = "JCLUSER"
                       OR XAi-current-user = "JESUSER"
                          MOVE SPACES TO ws-temp-2
                       ELSE
                          MOVE XAi-current-user TO ws-temp-2
                       END-IF
                   WHEN "UID"
                       MOVE ws-ext-uid TO ws-temp-2
                   WHEN "PWD"
                       MOVE ws-ext-pwd TO ws-temp-2
                   WHEN OTHER
                       MOVE 1 TO parseErrorFlag
                       EXIT SECTION
                   END-EVALUATE
                   PERFORM VARYING ws-temp-len FROM 1 BY 1
                       UNTIL ws-temp-2(ws-temp-len:1) = SPACE
                           OR ws-temp-2(ws-temp-len:1) = LOW-VALUE
                   END-PERFORM
                   SUBTRACT 1 FROM ws-temp-len
                   ADD 1 TO idx
                   IF idx > 256
                       MOVE 1 TO parseErrorFlag
                       EXIT SECTION
                   END-IF
                   MOVE ws-open-STRING(idx:) TO ws-temp
                   IF mPos + ws-temp-len > 256
                       MOVE 1 TO parseErrorFlag
                       EXIT SECTION
                   END-IF
                   MOVE ws-temp-2(1:ws-temp-len)
                       TO ws-open-STRING(mPos:ws-temp-len)
                   ADD ws-temp-len TO mPos
                   MOVE ws-temp TO ws-open-STRING(mPos:)
                   MOVE mPos TO idx
               ELSE
                   ADD 1 TO idx
               END-IF
           END-PERFORM
           exit.
           
       VaultExpandMacros SECTION.
           MOVE 0 TO parseErrorFlag
           MOVE 1 TO idx
           PERFORM UNTIL idx > 256 
                   OR ws-open-STRING(idx:1) = LOW-VALUE
               IF ws-open-STRING(idx:1) = MacroDelim
                   MOVE idx TO mPos
                   ADD 1 TO idx
                   IF idx > 256 OR ws-open-STRING(idx:1) = LOW-VALUES
                       MOVE 1 TO parseErrorFlag                  
                       EXIT SECTION
                   END-IF
                   IF ws-open-STRING(idx:1) = MacroDelim
                       *> MacroDelimMacroDelim expands to a single MacroDelim
                       MOVE ws-open-STRING(idx:) TO ws-temp
                       MOVE ws-temp TO ws-open-STRING(mPos:)
                       ADD 1 TO idx
                       EXIT PERFORM CYCLE
                   END-IF
                   MOVE 1 TO mx
                   INITIALIZE ws-temp
                   PERFORM UNTIL EXIT
                       MOVE ws-open-STRING(idx:) TO ws-temp(mx:1)
                       ADD 1 TO mx
                       ADD 1 TO idx
                       IF idx > 256 
                               OR ws-open-STRING(idx:1) = LOW-VALUES
                           MOVE 1 TO parseErrorFlag                  
                           EXIT SECTION
                       END-IF
                       IF ws-open-STRING(idx:1) = MacroDelim
                           EXIT PERFORM
                       END-IF
                   END-PERFORM
                   CALL "CBL_TOUPPER" USING 
                       ws-temp
                       BY VALUE mx
                   EVALUATE ws-temp
                   WHEN "PWD"
                       MOVE password TO ws-temp-2
                   WHEN OTHER
                       MOVE 1 TO parseErrorFlag
                       PERFORM VARYING ParmIx FROM 1 BY 1
                               UNTIL ParmIx > entriesUsed
                           if vName(ParmIx) = ws-temp
                               move 0 to parseErrorFlag
                               move vValue(ParmIx) to ws-temp-2
                               exit perform
                           end-if
                       END-PERFORM
                       IF parseErrorFlag not = 0
                           MOVE 1 TO MsgLen
                           MOVE 1 TO MsgLevel
                           STRING
                               ResName(ConnIX) delimited space
                               ": " delimited size
                               "Could not match "
                                       delimited size
                               "customization parameter "
                                       delimited size
                               ws-temp delimited space
                           INTO consoleMessage pointer MsgLen
                           SUBTRACT 1 FROM MsgLen
                           CALL "mfxaLogConsoleMsg" USING 
                              consoleMessage MsgLen 
                              EsLoggingLevel(ConnIX) MsgLevel
                           END-CALL 
                           EXIT SECTION
                       END-IF
                   END-EVALUATE
                   PERFORM VARYING ws-temp-len FROM 1 BY 1
                       UNTIL ws-temp-2(ws-temp-len:1) = SPACE
                           OR ws-temp-2(ws-temp-len:1) = LOW-VALUE
                   END-PERFORM
                   SUBTRACT 1 FROM ws-temp-len
                   ADD 1 TO idx
                   IF idx > 256
                       MOVE 1 TO parseErrorFlag                  
                       EXIT SECTION
                   END-IF
                   MOVE ws-open-STRING(idx:) TO ws-temp
                   IF mPos + ws-temp-len > 256
                       MOVE 1 TO parseErrorFlag                  
                       EXIT SECTION
                   END-IF                   
                   MOVE ws-temp-2(1:ws-temp-len) 
                       TO ws-open-STRING(mPos:ws-temp-len)
                   ADD ws-temp-len TO mPos
                   MOVE ws-temp TO ws-open-STRING(mPos:)
                   MOVE mPos TO idx
               ELSE
                   ADD 1 TO idx
               END-IF 
           END-PERFORM
           .


