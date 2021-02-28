       
       01  xaCustomizationInfo.
           03  returnCode              pic 9(4) comp-5.
           03  reasonCode              pic 9(4) comp-5.
           03  xaOpenString            pic x(256).
           03  password                pic x(32).
           03  entriesUsed             pic 9(4) comp-5.
           03  nameValuePairs occurs 64.
               05  vName               pic x(16).  *> space terminated
               05  vValue              pic x(256). *> null  terminated


