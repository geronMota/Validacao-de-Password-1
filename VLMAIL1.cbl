      ******************************************************************
      * Author:JEFEFRSON MOTA(GERO)
      * Date:26/03/23
      * Purpose:VALIDACAO DE PASSWORD
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. VLMAIL1.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77 VERIFICA                              PIC 9.
        88 VERIFICA-OK                          VALUE "S" FALSE "N".
       77   CONTADOR                            PIC 9.
       77  CARACTER                             PIC 9(1).
       77 LETRA-MAIUSC                          PIC 9(1).
       77 LETRA-MINUSC                          PIC 9(1).
       77 NUMERO                                PIC 9(1).


       77 WS-USER-PASSWORD                      PIC X(8).
       PROCEDURE DIVISION.

           P02-PASSWORD.

           DISPLAY "CADASTRE O PASSWORD: "
           ACCEPT  WS-USER-PASSWORD

           PERFORM VALIDAR-SENHA

           IF VERIFICA = 1
           DISPLAY "Senha valida!"

           PERFORM P02-PASSWORD
           END-IF.

           VALIDAR-SENHA.
                MOVE 0 TO LETRA-MAIUSC
                MOVE 0 TO LETRA-MINUSC
                MOVE 0 TO NUMERO
                MOVE 0 TO CARACTER
           INSPECT WS-USER-PASSWORD TALLYING LETRA-MAIUSC
           FOR ALL "A" ALL "B" ALL "C" ALL "D" ALL "E" ALL "F" ALL "G"
           ALL "H" ALL "I" ALL "J" ALL "L" ALL "M" ALL "N" ALL "O"
            ALL "P" ALL "Q" ALL "R" ALL "S" ALL "T" ALL "U" ALL "V"
            ALL "X" ALL "Z" ALL "W" ALL "Y" ALL "K"

            LETRA-MINUSC FOR ALL "a" ALL "b" ALL "c" ALL "d" ALL "e"
            ALL "f" ALL "g"  ALL "h" ALL "i" ALL "j" ALL "l" ALL "m"
            ALL "n" ALL "o"  ALL "p" ALL "q" ALL "r" ALL "s" ALL "t"
            ALL "u" ALL "v"  ALL "x" ALL "z" ALL "w" ALL "y" ALL "k"

           NUMERO FOR ALL "1" ALL "2" ALL "3" ALL "4" ALL "5" ALL "6"
           ALL "7" ALL "8" ALL "9" ALL "0"

            CARACTER FOR ALL "@" ALL "#" ALL "$" ALL "%" ALL "*" ALL "/"
            ALL "&"

           IF LETRA-MAIUSC >= 1 AND LETRA-MINUSC >= 1 AND NUMERO >= 1
           AND CARACTER >=1 AND LENGTH OF WS-USER-PASSWORD = 8
               DISPLAY "MAUISC " LETRA-MAIUSC
               DISPLAY "MINUSC " LETRA-MINUSC
               DISPLAY "NUMERO " NUMERO
               DISPLAY "CARACTERESPECIAL " CARACTER
               DISPLAY "ESSE EH LENGTH"LENGTH OF WS-USER-PASSWORD

           MOVE 1 TO VERIFICA
           ELSE
           DISPLAY "Senha invalida! Deve ter no maximo 8 caracteres "
           "pelo menos um 1 letra maiuscula,1 minuscula e 1 numero!"
               PERFORM P02-PASSWORD
           END-IF.


       P02-FIM.
            STOP RUN.
       END PROGRAM VLMAIL1.
