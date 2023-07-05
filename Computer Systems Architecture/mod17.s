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
even:   .quad   0x0f0f0f0f0f0f0f0f

        .text
        .globl  mod17
        .type   mod17, @function

/*
 * W moim rozwiązaniu używam następującej techniki:
        Zgodnie ze wskazówką, liczę różnicę między sumą heksadecymalnych cyfr na
        parzystych pozycjach i nieparzystych pozycjach + parę optymalizacji na liczbę instrukcji
        (komentarze w kodzie)
 */

mod17:
        /* mask out even and odd hex digits */
        movq    %rdi, %r8
        shrq    $4, %r8 
        andq    even, %rdi /* rdi = even digits */
        andq    even, %r8 /* r8 = odd digits */

        /* sum even digits */
        movq    %rdi, %r9
        shrq    $8, %r9 
        addq    %r9, %rdi 

        movq    %rdi, %r9 
        shrq    $16, %r9 
        addq    %r9, %rdi 

        movq    %rdi, %r9 
        shrq    $32, %r9 
        addq    %r9, %rdi

        /* sum odd */
        movq    %r8, %r10
        shrq    $8, %r10 
        addq    %r10, %r8 

        movq    %r8, %r10 
        shrq    $16, %r10 
        addq    %r10, %r8 

        movq    %r8, %r10 
        shrq    $32, %r10 
        addq    %r10, %r8

        /* calculate the difference of [even - odd]. 
           both sums are in [-60, 60] so we can calculate it on low-byte and 
           sign extend to whole register (sum \in [-120, 120]) */
        subb    %r8b, %dil
        movsbq   %dil, %rdi 

        /* if negative, add 136 -> diff in [0, 135] (2 hex digits), same remainder */
        sbbq    %r9, %r9
        andq    $136, %r9 
        addq    %r9, %rdi

        /* one last even - odd */
        movq    %rdi, %r9 
        shrq    $4, %r9 
        andq    $0xf, %rdi 
        subq    %r9, %rdi /* rdi \in [-15, 15] */

        /* if negative, add 17 -> rax \in [0, 16], same remainder (result) */
        sbbq    %rax, %rax 
        andq    $17, %rax 
        addq    %rdi, %rax
        ret

        .size   mod17, .-mod17
