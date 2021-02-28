//TESTSORT JOB 'JCL TEST',CLASS=A,MSGCLASS=A
//DEL1 EXEC  PGM=IEFBR14
//SORTOUT  DD  DSN=DAVS.SORTOUT,
//             DISP=(MOD,DELETE),SPACE=(TRK,(1,1))
//SYSOUT   DD  SYSOUT=*
//SYSPRINT DD  SYSOUT=*
/*
//DAVSSORT   EXEC PGM=SORT
//SYSOUT   DD  SYSOUT=*
//*        !!  WORK AREAS FOR SORT
//SORTWK01 DD  DSN=DAVS.WORK1,
//             UNIT=TEMP,
//             SPACE=(CYL,(20,5))
//SORTWK02 DD  DSN=DAVS.WORK2,
//             UNIT=TEMP,
//             SPACE=(CYL,(20,5))
//SORTWK03 DD  DSN=DAVS.WORK3,
//             UNIT=TEMP,
//             SPACE=(CYL,(20,5))
//SYSUT1   DD  *
//SYSIN    DD  *
  SORT FIELDS=(1,3,CH,A,4,60,CH,A)
  END
/*
//SORTIN   DD  *
This is line 1
This is line 2
This is line 3
This is line 4
This is line 5
This is line 6
//SORTOUT  DD  DSN=DAVS.SORTOUT,
//             DISP=(NEW,CATLG,DELETE),                                  
//             DCB=(RECFM=LSEQ,LRECL=80)                     
//
