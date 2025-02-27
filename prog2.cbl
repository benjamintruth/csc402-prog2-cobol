      ******************************************************************
      * Author: Rien Sanning, Arnav Shah, William Reynolds-Uti
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

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           *> Input Customers
           OPEN INPUT CUSTOMERS-FILE

           PERFORM UNTIL END-OF-FILE
               READ CUSTOMERS-FILE INTO CUSTOMER-RECORD
                   AT END MOVE 'Y' TO WS-EOF
               END-READ

               IF NOT END-OF-FILE
                   ADD 1 TO WS-CUSTOMER-COUNT
                   MOVE CUSTOMER-ID OF CUSTOMER-RECORD
                       TO CTABLE-CUSTOMER-ID(WS-CUSTOMER-COUNT)
                   MOVE CUSTOMER-NAME
                       TO CTABLE-CUSTOMER-NAME(WS-CUSTOMER-COUNT)
                   MOVE CUSTOMER-ADDRESS
                       TO CTABLE-CUSTOMER-ADDRESS(WS-CUSTOMER-COUNT)
                   MOVE CUSTOMER-CITY
                       TO CTABLE-CUSTOMER-CITY(WS-CUSTOMER-COUNT)
                   MOVE STATE-ZIP-COUNTRY
                       TO CTABLE-STATE-ZIP-COUNTRY(WS-CUSTOMER-COUNT)
                   MOVE AMOUNT-OWED
                       TO CTABLE-AMOUNT-OWED(WS-CUSTOMER-COUNT)
               END-IF
           END-PERFORM.

           CLOSE CUSTOMERS-FILE

           *> reset the EOF flag
           MOVE 'N' TO WS-EOF

           *> Input Inventory
           OPEN INPUT INVENTORY-FILE

           PERFORM UNTIL END-OF-FILE
               READ INVENTORY-FILE INTO INVENTORY-RECORD
                   AT END MOVE 'Y' TO WS-EOF
               END-READ

               IF NOT END-OF-FILE
                   ADD 1 TO WS-INVENTORY-COUNT
                   MOVE INVENTORY-ID OF INVENTORY-RECORD
                       TO ITABLE-INVENTORY-ID(WS-INVENTORY-COUNT)
                   MOVE ITEM-NAME
                       TO ITABLE-ITEM-NAME(WS-INVENTORY-COUNT)
                   MOVE IN-STOCK TO ITABLE-IN-STOCK(WS-INVENTORY-COUNT)
                   MOVE REORDER-POINT
                       TO ITABLE-REORDER-POINT(WS-INVENTORY-COUNT)
                   MOVE COST TO ITABLE-COST(WS-INVENTORY-COUNT)
               END-IF
           END-PERFORM.

           CLOSE INVENTORY-FILE.

           *> reset the EOF flag
           MOVE 'N' TO WS-EOF

           *> Input Transactions and process:
           OPEN INPUT TRANSACTION-FILE

           PERFORM UNTIL END-OF-FILE
               READ TRANSACTION-FILE INTO TRANSACTION-RECORD
                   AT END MOVE 'Y' TO WS-EOF
               END-READ

               IF NOT END-OF-FILE

                   *> GO ZONE

                   DISPLAY "Transaction: "
                   DISPLAY "Customer ID: " CUSTOMER-ID
                       OF TRANSACTION-RECORD

               *> IF ERROR OCCURS: PRINT TO ERROR, BREAK LOOP

               END-IF
           END-PERFORM.

           *> clean spacer
           DISPLAY "  "
           DISPLAY "  "
           DISPLAY "  "

           CLOSE TRANSACTION-FILE

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


       *>       test printing customer
                PERFORM VARYING CIDX FROM 1 BY 1
                  UNTIL CIDX > WS-CUSTOMER-COUNT
                    DISPLAY "Customer: "
                    DISPLAY "ID: " CTABLE-CUSTOMER-ID(CIDX)
                    DISPLAY "Name:" CTABLE-CUSTOMER-NAME(CIDX)
                END-PERFORM


       *>       test printing customer
                PERFORM VARYING IIDX FROM 1 BY 1
                  UNTIL IIDX > WS-INVENTORY-COUNT
                    DISPLAY "Inventory: "
                    DISPLAY "ID: " ITABLE-INVENTORY-ID(IIDX)
                    DISPLAY "Item Name:" ITABLE-ITEM-NAME(IIDX)

                END-PERFORM

               DISPLAY "END EXEC"
               STOP RUN.
       END PROGRAM PROG2.
