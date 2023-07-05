/*
 * UWAGA! W poniższym kodzie należy zawrzeć krótki opis metody rozwiązania
 *        zadania. Będzie on czytany przez sprawdzającego. Przed przystąpieniem
 *        do rozwiązywania zapoznaj się dokładnie z jego treścią. Poniżej należy
 *        wypełnić oświadczenie o samodzielnym wykonaniu zadania.
 *
 * Oświadczam, że zapoznałem(-am) się z regulaminem prowadzenia zajęć
 * i jestem świadomy(-a) konsekwencji niestosowania się do podanych tam zasad.
 *
 * Imię i nazwisko, numer indeksu: Mateusz Łuczyński, 331826
 */

       .data
M16: .quad 0xffffffff0000ffff
M8:  .quad 0x00ff00ff00ff00ff
M4:  .quad 0x0f0f0f0f0f0f0f0f
M2:  .quad 0x3333333333333333
M1:  .quad 0x5555555555555555

        .text
        .globl  bitrev
        .type bitrev, @function

/*
 * W moim rozwiązaniu używam następującej techniki: 
        Korzystam z algorytmu zamieniającego parami (w sposób równoległy) najpierw dwa bloki
        po 32 bity, potem 4 bloki po 16, ..., 32 bloki po 1 bit.
        Przykład na mniejszej ilości bitów (8):
        abcdefgh -> efgh abcd -> gh ef cd ab -> h g f e d c b a -> hgfedcba
 */

bitrev:
        mov     %rdi, %rax

        /* swap blocks of 32 bits */
        ror     $32, %rax

        /* swap blocks of 16 bits */
        mov     %rax, %rdx 
        shr     $16, %rdx 
        and     M16, %rdx 
        and     M16, %rax
        shl     $16, %rax 
        or      %rdx, %rax 

        /* swap blocks of 8 bits */
        mov     %rax, %rdx 
        shr     $8, %rdx 
        and     M8, %rdx 
        and     M8, %rax 
        shl     $8, %rax 
        or      %rdx, %rax

        /* swap blocks of 4 bits */
        mov     %rax, %rdx 
        shr     $4, %rdx 
        and     M4, %rdx 
        and     M4, %rax 
        shl     $4, %rax 
        or      %rdx, %rax

        /* swap blocks of 2 bits */
        mov     %rax, %rdx 
        shr     $2, %rdx 
        and     M2, %rdx 
        and     M2, %rax 
        lea     (%rdx, %rax, 4), %rax 

        /* swap blocks of 1 bit */
        mov     %rax, %rdx 
        shr     $1, %rdx 
        and     M1, %rdx 
        and     M1, %rax 
        lea     (%rdx, %rax, 2), %rax 

        ret

        .size bitrev, .-bitrev

