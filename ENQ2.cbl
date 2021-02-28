      $set CICSECM
       identification division.
       program-id. ENQ2.

       environment division.
       configuration section.

       data division.
       working-storage section.
       01  ws-enq1     pic x(10) value "ENQ1".
       01  ws-enq2     pic x(10) value "ENQ2".
       01  ws-len      pic 9(4) comp-3. 

       procedure division.
       
           move length of ws-enq1 to ws-len
           
      *    perform 10 times
           
               perform do-enq2
               exec cics 
                   delay for seconds(10)
               end-exec
      *        call "CBL_THREAD_SLEEP" using by value 10000
               perform do-enq1
               
      *        exec cics 
      *            delay for seconds(10)
      *        end-exec
               
               perform do-deq1      
               perform do-deq2
       
      *    end-perform               
       
           goback.

       
       do-enq1 section.
       
      ***** Issue an ENQ on a Resource to sync access
       
       
           exec cics
               enq resource(ws-enq1)
                   length(ws-len)
           end-exec
           .

       do-enq2 section.
       
      ***** Issue an ENQ on a Resource to sync access
       
       
           exec cics
               enq resource(ws-enq2)
                   length(ws-len)
           end-exec
           .


       
       do-deq1 section.
       
      ***** Issue an ENQ on a Resource to sync access
       
       
           exec cics
               deq resource(ws-enq1)
                   length(ws-len)
           end-exec
           .
       
       do-deq2 section.
       
      ***** Issue an ENQ on a Resource to sync access
       
       
           exec cics
               deq resource(ws-enq2)
                   length(ws-len)
           end-exec
           .       
