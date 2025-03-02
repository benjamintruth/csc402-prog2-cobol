      ******************************************************************
      * Author: Rien Sanning, Arnav Shah,
      * Date: 24-02-25
      * Purpose: Program 2, CSC 407
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG2.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           *> build input files
           SELECT CUSTOMERS-FILE ASSIGN TO 'customers.dat'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT INVENTORY-FILE ASSIGN TO 'inventory.dat'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT TRANSACTION-FILE ASSIGN TO 'transactions.dat'
               ORGANIZATION IS LINE SEQUENTIAL.
           *> output files
           SELECT INVOICE-FILE ASSIGN TO 'invoices.txt'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT ERROR-FILE ASSIGN TO 'errors.txt'
               ORGANIZATION IS LINE SEQUENTIAL.


       DATA DIVISION.
       FILE SECTION.
           FD  CUSTOMERS-FILE.
           01  CUSTOMER-RECORD.
               05  CUSTOMER-ID         PIC X(5).
               05  FILLER              PIC X(5).
               05  CUSTOMER-NAME       PIC X(18).
               05  CUSTOMER-ADDRESS    PIC X(20).
               05  CUSTOMER-CITY       PIC X(12).
               05  STATE-ZIP-COUNTRY   PIC X(12).
               05  AMOUNT-OWED         PIC 999.99.

           FD  INVENTORY-FILE.
           01  INVENTORY-RECORD.
               05  INVENTORY-ID        PIC X(6).
               05  FILLER              PIC X(5).
               05  ITEM-NAME           PIC X(22).
               05  FILLER              PIC X(2).
               05  IN-STOCK            PIC 99.
               05  FILLER              PIC X(5).
               05  REORDER-POINT       PIC 99.
               05  FILLER              PIC X(5).
               05  COST                PIC 99.99.

           FD  TRANSACTION-FILE.
           01  TRANSACTION-RECORD.
               05  CUSTOMER-ID         PIC X(5).
               05  FILLER              PIC X(5).
               05  INVENTORY-ID        PIC X(6).
               05  FILLER              PIC X(5).
               05  NUMBER-ORDERED      PIC 99.
               05  FILLER              PIC X(5).
               05  DISCOUNT            PIC X(1).

           FD INVOICE-FILE.
           01 INVOICE-RECORD.
              05 INV-CUSTOMER-NAME PIC X(18).
              05 FILLER PIC X(2) VALUE SPACES.
              05 INV-ITEM-NAME PIC X(22).
              05 FILLER PIC X(2) VALUE SPACES.
              05 INV-ITEM-COST PIC $$$,$$9.99.
              05 FILLER PIC X(2) VALUE SPACES.
              05 INV-NUMBER-ORDERED PIC Z9.
              05 FILLER PIC X(2) VALUE SPACES.
              05 INV-TOTAL-BEFORE-DISCOUNT PIC $$$,$$9.99.
              05 FILLER PIC X(2) VALUE SPACES.
              05 INV-DISCOUNT-APPLIED PIC X(3).
              05 FILLER PIC X(2) VALUE SPACES.
              05 INV-TOTAL-AFTER-DISCOUNT PIC $$$,$$9.99.

           FD ERROR-FILE.
           01 ERROR-RECORD.
              05 ERROR-LITERAL PIC X(6) VALUE "Error:".
              05 FILLER PIC X(1) VALUE SPACE.
              05 ERROR-TYPE PIC X(11).
              05 FILLER PIC X(1) VALUE SPACE.
              05 ERROR-ID PIC X(6).


       WORKING-STORAGE SECTION.

    01 WS-EOF        PIC X VALUE 'N'.
        88 END-OF-FILE VALUE 'Y'.

    *> array of customer records
    01 WS-CUSTOMER-COUNT  PIC 9(3) VALUE 0.
    01 CUSTOMER-TABLE.
      05 CUSTOMER-ENTRY OCCURS 100 TIMES INDEXED BY CIDX.
        10  CTABLE-CUSTOMER-ID       PIC X(5).
        10  CTABLE-CUSTOMER-NAME     PIC X(18).
        10  CTABLE-CUSTOMER-ADDRESS  PIC X(20).
        10  CTABLE-CUSTOMER-CITY     PIC X(12).
        10  CTABLE-STATE-ZIP-COUNTRY PIC X(12).
        10  CTABLE-AMOUNT-OWED       PIC 999.99.

    01 WS-INVENTORY-COUNT PIC 9(3) VALUE 0.
    01 INVENTORY-TABLE.
      05 INVENTORY-ENTRY OCCURS 100 TIMES INDEXED BY IIDX.
        10 ITABLE-INVENTORY-ID PIC X(6).
        10 ITABLE-ITEM-NAME PIC X(22).
        10 ITABLE-IN-STOCK PIC 99.
        10 ITABLE-REORDER-POINT PIC 99.
        10 ITABLE-COST PIC 99.99.
        
    *> Add these new variables here
    01 WS-CUSTOMER-INDEX    PIC 9(3) VALUE 0.
    01 WS-FOUND             PIC X VALUE 'N'.
        88 FOUND-ITEM       VALUE 'Y'.


       PROCEDURE DIVISION.
       MAIN-PROCEDURE.


           *> Step 1: Read Customers into Memory
           OPEN INPUT CUSTOMERS-FILE

           PERFORM UNTIL END-OF-FILE
               READ CUSTOMERS-FILE INTO CUSTOMER-RECORD
                   AT END MOVE 'Y' TO WS-EOF
               END-READ

               IF NOT END-OF-FILE
                   ADD 1 TO WS-CUSTOMER-COUNT
                   MOVE CUSTOMER-ID TO TABLE-CUSTOMER-ID(WS-CUSTOMER-COUNT)
                   MOVE CUSTOMER-NAME TO TABLE-CUSTOMER-NAME(WS-CUSTOMER-COUNT)
                   MOVE CUSTOMER-ADDRESS TO TABLE-CUSTOMER-ADDRESS(WS-CUSTOMER-COUNT)
                   MOVE CUSTOMER-CITY TO TABLE-CUSTOMER-CITY(WS-CUSTOMER-COUNT)
                   MOVE STATE-ZIP-COUNTRY TO TABLE-STATE-ZIP-COUNTRY(WS-CUSTOMER-COUNT)
                   MOVE AMOUNT-OWED TO TABLE-AMOUNT-OWED(WS-CUSTOMER-COUNT)
               END-IF
           END-PERFORM.
           CLOSE CUSTOMERS-FILE.

           *> Step 2: Read Inventory into Memory
           OPEN INPUT INVENTORY-FILE

           PERFORM UNTIL END-OF-FILE
               READ INVENTORY-FILE INTO INVENTORY-RECORD
                   AT END MOVE 'Y' TO WS-EOF
               END-READ

               IF NOT END-OF-FILE
                   ADD 1 TO WS-INVENTORY-COUNT
                   MOVE INVENTORY-ID TO TABLE-INVENTORY-ID(WS-INVENTORY-COUNT)
                   MOVE ITEM-NAME TO TABLE-ITEM-NAME(WS-INVENTORY-COUNT)
                   MOVE IN-STOCK TO TABLE-IN-STOCK(WS-INVENTORY-COUNT)
                   MOVE REORDER-POINT TO TABLE-REORDER-POINT(WS-INVENTORY-COUNT)
                   MOVE COST TO TABLE-COST(WS-INVENTORY-COUNT)
               END-IF
           END-PERFORM.
           CLOSE INVENTORY-FILE.

           *> Step 3: Process Transactions & Update Inventory
           OPEN INPUT TRANSACTION-FILE
           OPEN OUTPUT INVOICE-FILE
           OPEN OUTPUT ERROR-FILE

           PERFORM UNTIL END-OF-FILE
               READ TRANSACTION-FILE INTO TRANSACTION-RECORD
                   AT END MOVE 'Y' TO WS-EOF
               END-READ

               IF NOT END-OF-FILE
                   *> Step 3.1: Find Customer
                   SEARCH TABLE-CUSTOMER-ID
                       WHEN TABLE-CUSTOMER-ID(IDX) = CUSTOMER-ID
                           MOVE TABLE-CUSTOMER-NAME(IDX) TO INV-CUSTOMER-NAME
                   END-SEARCH

                   *> Step 3.2: Find Inventory Item
                   SEARCH TABLE-INVENTORY-ID
                       WHEN TABLE-INVENTORY-ID(IDX) = INVENTORY-ID
                           IF NUMBER-ORDERED > TABLE-IN-STOCK(IDX)
                               *> Not enough stock - Log an error
                               MOVE "Low Stock" TO ERROR-TYPE
                               MOVE INVENTORY-ID TO ERROR-ID
                               WRITE ERROR-RECORD
                           ELSE
                               *> Deduct stock and process transaction
                               SUBTRACT NUMBER-ORDERED FROM TABLE-IN-STOCK(IDX)

                               *> If below reorder point, log a warning
                               IF TABLE-IN-STOCK(IDX) < TABLE-REORDER-POINT(IDX)
                                   MOVE "Reorder Item" TO ERROR-TYPE
                                   MOVE INVENTORY-ID TO ERROR-ID
                                   WRITE ERROR-RECORD
                               END-IF

                               *> Generate invoice
                               MOVE TABLE-ITEM-NAME(IDX) TO INV-ITEM-NAME
                               MOVE TABLE-COST(IDX) TO INV-ITEM-COST
                               COMPUTE INV-TOTAL-BEFORE-DISCOUNT = TABLE-COST(IDX) * NUMBER-ORDERED
                               MOVE "Yes" TO INV-DISCOUNT-APPLIED
                               COMPUTE INV-TOTAL-AFTER-DISCOUNT = INV-TOTAL-BEFORE-DISCOUNT * 0.90
                               WRITE INVOICE-RECORD
                           END-IF
                   END-SEARCH
               END-IF
           END-PERFORM.

           *> Step 4: Close Files
           CLOSE TRANSACTION-FILE
           CLOSE INVOICE-FILE
           CLOSE ERROR-FILE

           DISPLAY "Processing Complete."
           STOP RUN.


               *> example data testing records
           OPEN INPUT CUSTOMERS-FILE

           PERFORM UNTIL END-OF-FILE
               READ CUSTOMERS-FILE INTO CUSTOMER-RECORD
                   AT END MOVE 'Y' TO WS-EOF
               END-READ

               IF NOT END-OF-FILE
                   ADD 1 TO WS-CUSTOMER-COUNT
                   MOVE CUSTOMER-ID OF CUSTOMER-RECORD
                       TO TABLE-CUSTOMER-ID(WS-CUSTOMER-COUNT)
                   MOVE CUSTOMER-NAME
                       TO TABLE-CUSTOMER-NAME(WS-CUSTOMER-COUNT)
                   MOVE CUSTOMER-ADDRESS
                       TO TABLE-CUSTOMER-ADDRESS(WS-CUSTOMER-COUNT)
                   MOVE CUSTOMER-CITY
                       TO TABLE-CUSTOMER-CITY(WS-CUSTOMER-COUNT)
                   MOVE STATE-ZIP-COUNTRY
                       TO TABLE-STATE-ZIP-COUNTRY(WS-CUSTOMER-COUNT)
                   MOVE AMOUNT-OWED
                       TO TABLE-AMOUNT-OWED(WS-CUSTOMER-COUNT)
               END-IF
           END-PERFORM.

           CLOSE CUSTOMERS-FILE


               *> testing printing records from data division
               DISPLAY " "

               MOVE "INV001" TO INVENTORY-ID OF INVENTORY-RECORD
               MOVE "Laptop Computer" TO ITEM-NAME
               MOVE 15 TO IN-STOCK
               MOVE 05 TO REORDER-POINT
               MOVE 99.99 TO COST
               DISPLAY "Inventory Record: " INVENTORY-RECORD


               DISPLAY " "


               MOVE "C1001" TO CUSTOMER-ID OF TRANSACTION-RECORD
               MOVE "INV001" TO INVENTORY-ID OF TRANSACTION-RECORD
               MOVE 02 TO NUMBER-ORDERED
               MOVE "N" TO DISCOUNT
               DISPLAY "Transaction Record: " TRANSACTION-RECORD

               *> going to do a small example of the output files

               *> make an invoice
               OPEN OUTPUT INVOICE-FILE

               MOVE "John Smith" TO INV-CUSTOMER-NAME
               MOVE "Laptop Computer" TO INV-ITEM-NAME
               MOVE 99.99 TO INV-ITEM-COST
               MOVE 02 TO INV-NUMBER-ORDERED
               MOVE 199.98 TO INV-TOTAL-BEFORE-DISCOUNT
               MOVE "Yes" TO INV-DISCOUNT-APPLIED
               MOVE 179.98 TO INV-TOTAL-AFTER-DISCOUNT
               WRITE INVOICE-RECORD

               CLOSE INVOICE-FILE

               *> error FILE
               OPEN OUTPUT ERROR-FILE

               MOVE "Customer ID" TO ERROR-TYPE
               MOVE "C9999" TO ERROR-ID
               WRITE ERROR-RECORD

               CLOSE ERROR-FILE


               PERFORM VARYING IDX FROM 1 BY 1
                 UNTIL IDX > WS-CUSTOMER-COUNT

                   DISPLAY "ID: " TABLE-CUSTOMER-ID(IDX)
                   DISPLAY "Name:" TABLE-CUSTOMER-NAME(IDX)

               END-PERFORM





               DISPLAY "END EXEC"
               STOP RUN.


*> New subroutine to process transactions
       PROCESS-TRANSACTION.
           *> Find customer in the customer table

               *> Find customer in the customer table
MOVE 'N' TO WS-FOUND
PERFORM VARYING CIDX FROM 1 BY 1
  UNTIL CIDX > WS-CUSTOMER-COUNT OR FOUND-ITEM
    IF CTABLE-CUSTOMER-ID(CIDX) = CUSTOMER-ID OF TRANSACTION-RECORD
        MOVE 'Y' TO WS-FOUND
        MOVE CIDX TO WS-CUSTOMER-INDEX
    END-IF
END-PERFORM

*> If customer not found, write error and exit
IF NOT FOUND-ITEM
    MOVE "Customer ID" TO ERROR-TYPE
    MOVE CUSTOMER-ID OF TRANSACTION-RECORD TO ERROR-ID
    WRITE ERROR-RECORD
    EXIT PARAGRAPH
END-IF


       END PROGRAM PROG2.
