      $set cicsecm
       identification division.
       program-id. MSG1.

       environment division.
       configuration section.

       data division.
       working-storage section.
       01  ws-msg                       pic x(40) value
                                        "This is Message 1 from MSG1".

       procedure division.

           exec cics
               send text from(ws-msg)
           end-exec
           exec cics
               send text from(eibtrmid)
           end-exec
           exec cics send control freekb end-exec
           exec cics return end-exec

           goback.
           
       end program MSG1.
