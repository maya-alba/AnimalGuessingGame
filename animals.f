      PROGRAM animals

      IMPLICIT NONE
C  THis program can store 100 animals
      INTEGER maxNodes, U
      parameter (maxNodes=201, U=8)

      CHARACTER answ*3, answYN*3, again*3
      CHARACTER (15) :: animalUser=""
      CHARACTER (15) :: tmpOldAni=""
      CHARACTER (15) :: tmpNewAni=""
      CHARACTER (50) :: questUser=""
      CHARACTER (50) :: lastQuest=""
      CHARACTER (60) :: diffQuest=""
      CHARACTER (50) :: quest=""
      CHARACTER (20) :: fileName=""
      
      LOGICAL seguir, CheckType, fileExists

C  Array of characters to store question | animal
      CHARACTER questions(maxNodes,2)*50          
C  Array of integers to store type | no node | yes node 
      INTEGER nodes(maxNodes,3), colQ, colN, R, P, L, I
      INTEGER oldLength, newLength, newArt, oldArt, tmpL1, tmpL2
      INTEGER numNodes, root, actualNode, pastNode, tmp, newNode
C  Declaration of functions used
      INTEGER InitNodes, InitQuest, PrintQuest,PrintNodes,CheckYN,strlen

      newNode = -1
      colQ = 2
      colN = 3
      newArt=-1
      oldArt=-1
      seguir = .TRUE.

C  Initialize questions and nodes arrays
      R = InitQuest(questions,maxNodes,colQ)
      R =InitNodes(nodes,maxNodes,colN)

C  Ask User if he wants to load existing file
      WRITE(*,'(a$)') "Enter saved game file (or NO for no file): "
      READ(*,*) fileName

      R=checkYN(fileName)
      IF(R.EQ.2) THEN
        PRINT*, "Using/creating default file"
        fileName="stored.dat"
        INQUIRE( FILE=fileName, EXIST=fileExists)
        GO TO 2
      END IF

      INQUIRE(FILE=fileName, EXIST=fileExists)
      
      IF(.NOT.fileExists) THEN
        PRINT*, "File was not found, using default file"
        fileName="stored.dat"
        INQUIRE( FILE=fileName, EXIST=fileExists)
      END IF      


C  Check if file exists      
C      INQUIRE( FILE=fileName, EXIST=fileExists)

 2    IF(fileExists) THEN
        OPEN(U, FILE=fileName, STATUS='OLD')
        READ(U,*) root
        READ(U,*) numNodes
C  Retrieve questions
        DO I=1, numNodes
          READ(U,'(A)') questions(I,1)
        END DO
C  Retrieve animals
        DO I=1,numNodes,2
          READ(U,'(A)') questions(I,2)
        END DO
C  Retrieve nodes
        DO I=1, numNodes
          READ(U,'(3I12)') nodes(I,1), nodes(I,2), nodes(I,3)
        END DO
        WRITE(*,*) numNodes/2+1," animals loaded!"

C  File doesn't exist, initialize data
      ELSE
        OPEN(U, FILE=fileName, STATUS='NEW')
        WRITE(U,*) 1
        root = 1
        WRITE(U,*) 1
        numNodes = 1

        questions(1,1) = "Is your animal a horse?"
        questions(1,2) = "horse"
        WRITE(U,'(A)') questions(1,1)
        WRITE(U,'(A)') questions(1,2)
        nodes(1,1) = 1
        nodes(1,2) = -1
        nodes(1,3) = -1
        WRITE(U,*) nodes(1,1), nodes(1,2), nodes(1,3)
        PRINT*, "(1 animal loaded!)"
 
      END IF
      CLOSE(U)    
      
 1    PRINT *, "Think of an animal and then press ENTER to continue:"
      READ(*,*)
      
      pastNode = 0
      actualNode = root
      seguir=.TRUE.
      newLength=0
      oldLength=0

C  Advance until user faces last question type
      DO while (seguir) 

 10     WRITE(*,'(a$)') questions(actualNode,1)
        READ(*,*) answ
C  Check what the user typed Yes/No/Invalid
        R=CheckYN(answ)
C  If answer is invalid ask question again
        IF(R.EQ.-1) THEN
          PRINT*, "That answer is invalid (try yes/no)"
          GO TO 10
        END IF
C  Check if node is a last question type
        seguir = CheckType(nodes,actualNode)

        IF(seguir) THEN
          tmp = actualNode
          actualNode= nodes(actualNode,R)
          pastNode = tmp
        END IF

      END DO

C  Program did't guess the animal
      IF(R.EQ.2) THEN
        WRITE(*,*) "What is your animal?"
        READ(*,'(A)') animalUser 
 
C  See if article is used
        CALL checkArt(animalUser,newArt)
 
C  Calculate length of already stored animal
        CALL shortS(questions(actualNode,2),1,oldLength)
C Calculate length of new animal, trim article off
        CALL shortS(animalUser,newArt,newLength)        
        animalUser=animalUser(newArt:newArt+newLength)

        CALL checkOldArt(questions(actualNode,2),oldArt)
 
        CALL addArticle(questions(actualNode,2),oldLength,oldArt,
     +                   tmpOldAni)
        tmpL1=oldLength+oldArt-1
C  Convert string to lowercase
        CALL lowerCase(animalUser,newLength)

C  Give correct article to animal
          CALL typeArt(animalUser,newArt)

        CALL addArticle(animalUser,newLength,newArt,
     +                   tmpNewAni)
        tmpL2=newLength+newArt-1

C  Ask for differentiator question
        CALL differentiator(diffQuest,tmpOldAni,tmpL1,
     +                      tmpNewAni,tmpL2)
        WRITE(*,*) diffQuest
        READ(*,'(A)') questUser
C  Check if question typed by user has capital letter and '?'
        L=strlen(questUser)
        CALL checkGrammar(questUser,L)


 20     quest="For "//tmpNewAni(1:tmpL2)//" the answer would be?"
        WRITE(*,'(a$)') quest
        READ(*,*) answYN

C  Check if answer is valid
        R= checkYN(answYN)
        IF(R.EQ.-1) THEN
          PRINT*,"That answer is invalid (try yes/no)"
          GO TO 20
        END IF

C  Store diffQuest in array of quest
        numNodes = numNodes+1
        newNode = numNodes
        questions(newNode,1) = questUser
C  Store info of new node
        nodes(newNode,1) = 0
        nodes(newNode,R) = newNode+1
        IF(R.EQ.2) THEN
          nodes(newNode,3) = actualNode
        ELSE
          nodes(newNode,2) = actualNode
        END IF

C  Add link to new node
        IF(pastNode.NE.0) THEN
          IF(R.EQ.2) THEN
            nodes(pastNode,3) = actualNode
          ELSE
            nodes(pastNode,2)=newNode
          END IF
        END IF


C  Store new last question type in array of quest and nodes
        numNodes=numNodes+1
        CALL lastQuestion(lastQuest,tmpNewAni(1:tmpL2),tmpL2)
        questions(numNodes,1)=lastQuest
        questions(numNodes,2)=animalUser(1:newLength)
        nodes(numNodes,1)=1

        IF(actualNode.EQ.root) THEN
          root=newNode
        END IF

 
        WRITE(*,*)
C        P=PrintQuest(questions,maxNodes,colQ)
C        P=PrintNodes(nodes,maxNodes,colN)
        

C  Program guessed the animal
      ELSE
        PRINT*,"I won!! :)"    
      END IF

 30   WRITE(*,'(a$)') "Thank you. Play again? "
      READ(*,*) again
      WRITE(*,*)

C  Check what the user typed Yes/No/Invalid
      R=CheckYN(again)
C  If answer is invalid ask question again
      IF(R.EQ.-1) THEN
        PRINT*, "That answer is invalid (try yes/no)"
        GO TO 30
      ELSE IF(R.EQ.3) THEN
        GO TO 1
      ELSE
        GO TO 100
      END IF

 100  PRINT*,"Storing animals..."
      OPEN(U, FILE=fileName, STATUS='OLD')
      WRITE(U,*) root
      WRITE(U,*) numNodes
C Store questions
      DO I=1, numNodes
        WRITE(U,'(A)') questions(I,1)
      END DO
C Store animals
      DO I=1,numNodes,2
        WRITE(U,'(A)') questions(I,2)
      END DO
C  Store nodes
      DO I=1, numNodes
         WRITE(U,*) nodes(I,1), nodes(I,2), nodes(I,3)
      END DO
      CLOSE(U)

      WRITE(*,*) numNodes/2+1," animals stored!"

C  END of MAIN
      END



C  Initializing Nodes Array
      INTEGER FUNCTION InitNodes(A,M,N)
      INTEGER A(M,N), ROW, COL
      DO ROW = 1, M
         DO  COL = 1, N
           A(ROW,COL)=-1
         END DO
      END DO
      END

C  Initializing Questions Array
      INTEGER FUNCTION InitQuest(A,M,N)
      CHARACTER A(M,N)*50
      INTEGER ROW, COL
      DO ROW = 1, M
         DO  COL = 1, N
           A(ROW,COL)="-"
         END DO
      END DO
      END

C  Print Questions Array
      INTEGER FUNCTION PrintQuest(A,M,N)
      CHARACTER A(M,N)*50
      INTEGER ROW, COL
      PRINT*, "Questions Array"
      DO ROW = 1, M !k=rows
         PRINT*, (A(ROW,COL), COL = 1, N) !j=columns
      END DO
      END

C  Print Nodes Array
      INTEGER FUNCTION PrintNodes(A,M,N)
      INTEGER A(M,N), ROW, COL
      PRINT*, "Nodes Array"
      DO ROW = 1, M !k=rows
         PRINT*, (A(ROW,COL), COL = 1, N) !j=columns
      END DO
      END

C  Check if YES=3, NO=2 or invalid=-1
      INTEGER FUNCTION CheckYN(A)
      CHARACTER A*3
      INTEGER R
      IF ((A.EQ."y  ").OR.(A.EQ."Y  ").OR.(A.EQ."YES").OR.
     +	(A.EQ."yes").OR.(A.EQ."Yes")) THEN
      	   CheckYN=3
      ELSE IF ((A.EQ."n  ").OR.(A.EQ."N  ").OR.(A.EQ."NO ").OR.
     +	(A.EQ."no ").OR.(A.EQ."No ")) THEN
      	  CheckYN= 2	
      ELSE
     	CheckYN= -1
      END IF
      RETURN
      END

C  Check type of node
C  If node is a last question type 'Is it a horse?' 'Is it a bird?' = FALSE
      LOGICAL FUNCTION CheckType(A,M)
      INTEGER A(M,1), M
      IF((A(M,1)).EQ.(1)) THEN
        CheckType=.FALSE.
      ELSE
        CheckType=.TRUE.
      END IF
      RETURN
      END

C  Receive animal and decide what type of article it needs
C  a=3 an=4
      SUBROUTINE checkOldArt(ani, art)
      CHARACTER(50) ani
      INTEGER art
      IF ((ani(1:1).EQ."a").OR.(ani(1:1).EQ."e").OR.(ani(1:1)
     +.EQ."i").OR.(ani(1:1).EQ."o").OR.(ani(1:1).EQ."u")) THEN
        art=4
      ELSE
        art=3
      END IF
      RETURN
      END

C  Receive animal and decide what type of article it needs
C  a=3 an=4
      SUBROUTINE typeArt(ani, art)
      CHARACTER(15) ani
      INTEGER art
      IF ((ani(1:1).EQ."a").OR.(ani(1:1).EQ."e").OR.(ani(1:1)
     +.EQ."i").OR.(ani(1:1).EQ."o").OR.(ani(1:1).EQ."u")) THEN
        art=4
      ELSE
        art=3
      END IF
      RETURN
      END

C Find short string
      SUBROUTINE shortS(ani,start, length)
      CHARACTER(15) ani
      INTEGER I, length, start
      DO I=start,15
        IF(ani(I:I).NE." ")THEN
          length = length+1
        END IF
      END DO
      RETURN
      END 

C Formulate differentiator question
      SUBROUTINE differentiator(quest, oldA, oldL, newA, newL)
      CHARACTER(60) quest
      CHARACTER(15) oldA, newA
      INTEGER oldL, newL
      quest="What question would distinguish "//oldA(1:oldL)
     +       //" from "//newA(1:newL)//"?"
      RETURN
      END

C Formulate differentiator question
      SUBROUTINE lastQuestion(lastQuest, animal, length)
      CHARACTER(50) lastQuest
      CHARACTER(15) animal
      INTEGER length
      lastQuest="Is your animal "//animal(1:length)//"?"
      RETURN
      END

C  Check if user inputs article+noun or just noun
C  IF it has a=3, an=4, no article=1
      SUBROUTINE checkArt(input,art)
      CHARACTER(15) input
      INTEGER art
      IF(input(2:2).EQ." ") THEN
        art=3
      ELSE IF(input(3:3).EQ." ") THEN
        art=4
      ELSE
        art=1
      END IF
      RETURN
      END  

C  Add article to animal
      SUBROUTINE addArticle(animal, length, art, tmp)
      CHARACTER(15) animal,tmp
      INTEGER art, length
      IF(art.EQ.4) THEN
        tmp = "an "//animal(1:length)
      ELSE
        tmp = "a "//animal(1:length)
      END IF
      RETURN
      END  

C  Find the last character of a string
      INTEGER FUNCTION strlen(st)
      INTEGER i
      CHARACTER	st*(*)
      i = LEN(st)
      DO WHILE (st(i:i) .EQ. ' ')
        i = i - 1
      ENDDO
      strlen = i
      RETURN
      END

C  Check if questions starts with capital letter and has '?'
      SUBROUTINE checkGrammar(quest, length)
      CHARACTER(50) quest
      INTEGER length, ascii
      IF(quest(length:length).NE."?") THEN
        quest(length+1:length+1)="?"
      END IF
      ascii= ICHAR(quest(1:1))
      IF (ascii.GT.96) THEN
        quest(1:1) = CHAR(ascii-32)
      END IF
      RETURN
      END

C  Convert string to pure lowercases
      SUBROUTINE lowerCase(ani, length)
      CHARACTER(15) ani
      INTEGER length, ascii, I
      DO I=1, length
        ascii= ICHAR(ani(I:I))
        IF (ascii.LT.91) THEN
          ani(I:I) = CHAR(ascii+32)
        END IF
      END DO
      RETURN
      END 
  
        


 

