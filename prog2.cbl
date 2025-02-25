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
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.


               *> example data testing records
               MOVE "C1001" TO CUSTOMER-ID OF CUSTOMER-RECORD
               MOVE "John Smith" TO CUSTOMER-NAME
               MOVE "123 Main Street" TO CUSTOMER-ADDRESS
               MOVE "New York" TO CUSTOMER-CITY
               MOVE "NY 10001 USA" TO STATE-ZIP-COUNTRY
               MOVE 125.50 TO AMOUNT-OWED
               DISPLAY "Customer Record: " CUSTOMER-RECORD

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



            DISPLAY "END EXEC"
            STOP RUN.
       END PROGRAM PROG2.
