caspac -aInitPac=MYPAC -sredis,127.0.0.1:6379 -nMYSOR
dbfhadmin -region -status -usedb:MYSERVER -name:CICSDB1
dbfhadmin -region -reset -usedb:MYSERVER -name:CICSDB2

