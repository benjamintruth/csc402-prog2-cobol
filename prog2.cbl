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
              05 INVO-CUSTOMER-NAME PIC X(18).
              05 FILLER PIC X(2) VALUE SPACES.
              05 INVO-ITEM-NAME PIC X(22).
              05 FILLER PIC X(2) VALUE SPACES.
              05 INVO-ITEM-COST PIC $$$,$$9.99.
              05 FILLER PIC X(2) VALUE SPACES.
              05 INVO-NUMBER-ORDERED PIC Z9.
              05 FILLER PIC X(2) VALUE SPACES.
              05 INVO-TOTAL-BEFORE-DISCOUNT PIC $$$,$$9.99.
              05 FILLER PIC X(2) VALUE SPACES.
              05 INVO-DISCOUNT-APPLIED PIC X(3).
              05 FILLER PIC X(2) VALUE SPACES.
              05 INVO-TOTAL-AFTER-DISCOUNT PIC $$$,$$9.99.

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

            01 WS-TEST-PROCESSING.
               05 TEST-NUMBER-ORDERED       PIC 99 VALUE 04.
               05 TEST-COST                 PIC 99V99 VALUE 10.00.
               05 TEST-ORDERCOST            PIC 99V99.
               05 TEST-DISCOUNT-CODE        PIC X(1) VALUE "Z".

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
               10 ITABLE-COST PIC 99V99.

           *> transaction inventory id index
           01 WS-INV-TRANS-IDX PIC 9(3) VALUE 0.
           01 WS-INV-ID-FOUND PIC X VALUE 'N'.
             88 FOUND-TRANS-INV VALUE 'Y'.


           01 WS-CUSTOMER-TRANS-IDX    PIC 9(3) VALUE 0.
           01 WS-CUST-ID-FOUND             PIC X VALUE 'N'.
             88 FOUND-TRANS-CUST       VALUE 'Y'.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           DISPLAY "  "
           DISPLAY "BEGIN PROGRAM"
           DISPLAY "  "


                          *> Try math
           MULTIPLY TEST-COST BY TEST-NUMBER-ORDERED
               GIVING TEST-ORDERCOST.

           EVALUATE TEST-DISCOUNT-CODE
               WHEN "A"
                   MULTIPLY TEST-ORDERCOST
                       BY 0.9 GIVING TEST-ORDERCOST
               WHEN "B"
                   MULTIPLY TEST-ORDERCOST
                       BY 0.8 GIVING TEST-ORDERCOST
               WHEN "C"
                   MULTIPLY TEST-ORDERCOST
                       BY 0.75 GIVING TEST-ORDERCOST
               WHEN "D"
                   IF TEST-NUMBER-ORDERED GREATER 3 THEN
                       SUBTRACT TEST-COST
                           FROM TEST-ORDERCOST GIVING TEST-ORDERCOST
                   END-IF
               WHEN "E"
                   IF TEST-NUMBER-ORDERED GREATER 2 THEN
                       SUBTRACT TEST-COST
                           FROM TEST-ORDERCOST GIVING TEST-ORDERCOST
                   END-IF.

           DISPLAY TEST-ORDERCOST.




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
           OPEN OUTPUT ERROR-FILE
           OPEN OUTPUT INVOICE-FILE

           PERFORM UNTIL END-OF-FILE
               READ TRANSACTION-FILE INTO TRANSACTION-RECORD
                   AT END MOVE 'Y' TO WS-EOF
               END-READ

               IF NOT END-OF-FILE

                   *> GO ZONE
                   DISPLAY "Transaction: "
                   DISPLAY "Customer ID: " CUSTOMER-ID
                       OF TRANSACTION-RECORD

                   *> CUSTOMER LOOKUP
                   MOVE 'N' TO WS-CUST-ID-FOUND
                   PERFORM VARYING CIDX FROM 1 BY 1
                   UNTIL CIDX > WS-CUSTOMER-COUNT OR FOUND-TRANS-CUST

                       IF CTABLE-CUSTOMER-ID(CIDX) = CUSTOMER-ID
                       OF TRANSACTION-RECORD

                           MOVE 'Y' TO WS-CUST-ID-FOUND
                           MOVE CIDX TO WS-CUSTOMER-TRANS-IDX
                       END-IF
                   END-PERFORM

                   *> If customer not found, write error and exit
                   IF NOT FOUND-TRANS-CUST


                       *> DEV : todo: REMOVE LATER
                       DISPLAY "CREATE TRANS CUST NOT FOUND ERROR"

                       *> write error record if cust not found
                       MOVE "Customer ID NOT FOUND" TO ERROR-TYPE
                       MOVE CUSTOMER-ID OF TRANSACTION-RECORD
                       TO ERROR-ID
                       WRITE ERROR-RECORD

                   END-IF


                    *> print transaction item name:
                    DISPLAY "Customer Name: "
                    DISPLAY CTABLE-CUSTOMER-NAME(WS-CUSTOMER-TRANS-IDX)


                   *> fetch inventory ID in the same pattern
                    MOVE 'N' TO WS-INV-ID-FOUND
                    PERFORM VARYING IIDX FROM 1 BY 1
                      UNTIL IIDX > WS-INVENTORY-COUNT

                       IF ITABLE-INVENTORY-ID(IIDX) =
                            INVENTORY-ID OF TRANSACTION-RECORD
                           *> store the index of this matching
                           *> inventory record
                           MOVE IIDX
                           TO WS-INV-TRANS-IDX

                           MOVE 'Y' TO WS-INV-ID-FOUND

                       END-IF
                    END-PERFORM

                    IF NOT FOUND-TRANS-INV

                        *> DEV : todo: REMOVE LATER
                       DISPLAY "CREATE TRANS INV NOT FOUND ERROR"

                       *> write error record
                       MOVE "Inventory ID NOT FOUND" TO ERROR-TYPE
                       MOVE INVENTORY-ID OF TRANSACTION-RECORD
                       TO ERROR-ID
                       WRITE ERROR-RECORD
                    END-IF


                    *> print transaction item name:
                    DISPLAY "Transaction Item: "
                    DISPLAY ITABLE-ITEM-NAME(WS-INV-TRANS-IDX)


                    IF FOUND-TRANS-CUST AND FOUND-TRANS-INV


                        *> step 1: create invoices and compute costs

                        *> step 2: modify inv and cust arrays through
                        *> program

                        MOVE CTABLE-CUSTOMER-NAME(WS-CUSTOMER-TRANS-IDX)
                        TO INVO-CUSTOMER-NAME

                        MOVE ITABLE-ITEM-NAME(WS-INV-TRANS-IDX)
                        TO INVO-ITEM-NAME

                        MOVE ITABLE-COST(WS-INV-TRANS-IDX)
                        TO INVO-ITEM-COST

                        MOVE NUMBER-ORDERED OF TRANSACTION-RECORD
                        TO INVO-NUMBER-ORDERED

                        MULTIPLY
                           NUMBER-ORDERED OF TRANSACTION-RECORD
                           BY
                           ITABLE-COST(WS-INV-TRANS-IDX)
                           GIVING INVO-TOTAL-BEFORE-DISCOUNT










      *>         05 INV-TOTAL-AFTER-DISCOUNT PIC $$$,$$9.99.


      *>         05 INV-DISCOUNT-APPLIED PIC X(3).




                    END-IF




                   *> spacer for clean printing
                   DISPLAY "   "
               END-IF
           END-PERFORM.

           *> clean spacer
           DISPLAY "  "
           DISPLAY "  "
           DISPLAY "  "

           CLOSE TRANSACTION-FILE
           CLOSE ERROR-FILE
           CLOSE INVOICE-FILE

               *> going to do a small example of the output files

               *> make an invoice
      *>          OPEN OUTPUT INVOICE-FILE

      *>          MOVE "John Smith" TO INV-CUSTOMER-NAME
      *>          MOVE "Laptop Computer" TO INV-ITEM-NAME
      *>          MOVE 99.99 TO INV-ITEM-COST
      *>          MOVE 02 TO INV-NUMBER-ORDERED
      *>          MOVE 199.98 TO INV-TOTAL-BEFORE-DISCOUNT
      *>          MOVE "Yes" TO INV-DISCOUNT-APPLIED
      *>          MOVE 179.98 TO INV-TOTAL-AFTER-DISCOUNT
      *>          WRITE INVOICE-RECORD

      *>          CLOSE INVOICE-FILE

               *> error FILE
      *>          OPEN OUTPUT ERROR-FILE

      *>          MOVE "Customer ID" TO ERROR-TYPE
      *>          MOVE "C9999" TO ERROR-ID
      *>          WRITE ERROR-RECORD

      *>          CLOSE ERROR-FILE


           *> LOOP EXAMPLES

       *>       test printing customer
                PERFORM VARYING CIDX FROM 1 BY 1
                  UNTIL CIDX > WS-CUSTOMER-COUNT
      *>               DISPLAY "Customer: "
      *>               DISPLAY "ID: " CTABLE-CUSTOMER-ID(CIDX)
      *>               DISPLAY "Name:" CTABLE-CUSTOMER-NAME(CIDX)
                END-PERFORM


       *>       test printing inventory
                PERFORM VARYING IIDX FROM 1 BY 1
                  UNTIL IIDX > WS-INVENTORY-COUNT
      *>               DISPLAY "Inventory: "
      *>               DISPLAY "ID: " ITABLE-INVENTORY-ID(IIDX)
      *>               DISPLAY "Item Name:" ITABLE-ITEM-NAME(IIDX)

                END-PERFORM

               DISPLAY "END EXEC"
               STOP RUN.




       END PROGRAM PROG2.
