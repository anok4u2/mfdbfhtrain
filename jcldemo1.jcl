//JCLDEMO1 JOB 'DEL CATALOG',CLASS=A 
//*
//* Delete file
//*
//STEP1 EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN DD *
  DELETE  MFDAVS.FILE001.BASE CLUSTER PURGE 
  SET MAXCC=0
/*
//*
//* Define the base cluster
//*
//STEP2 EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN DD *
  DEFINE CLUSTER ( NAME (MFDAVS.FILE002.BASE) -
                   RECORDSIZE(1047 1047) -
                   INDEXED KEYS (33 0) -
                   SHR (2) SPEED -
                   BUFSP(10240) -
                   REC (1047 1047) FSPC(5 1))-
            DATA  (NAME(MFDAVS.FILE002.BASE.DATA) CISZ(4096)) -
            INDEX (NAME(MFDAVS.FILE002.BASE.INDEX) CISZ(2048))
/*
//STEP2 EXEC PGM=MFWAIT
//SYSOUT  DD SYSOUT=*
//FILE001 DD DSN=MFDAVS.FILE002.BASE,DISP=OLD
//