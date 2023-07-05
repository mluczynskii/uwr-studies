/*
 * UWAGA! W poniższym kodzie należy zawrzeć krótki opis metody rozwiązania
 *        zadania. Będzie on czytany przez sprawdzającego. Przed przystąpieniem
 *        do rozwiązywania zapoznaj się dokładnie z jego treścią. Poniżej należy
 *        wypełnić oświadczenie o samodzielnym wykonaniu zadania.
 *
 * Oświadczam, że zapoznałem się z regulaminem prowadzenia zajęć
 * i jestem świadomy konsekwencji niestosowania się do podanych tam zasad.
 *
 * Imię i nazwisko, numer indeksu: Mateusz Łuczyński, 331826
 */

        .text
        .globl  clz
        .type   clz, @function

/*
 * W moim rozwiązaniu używam następującej techniki: 
        Na początku rozsmarowuje najstarszy zapalony bit na wszystkie bity po jego prawej stronie [O(logn)],
        następnie zliczam ilość zapalonych bitów w tak powstałej liczbie i odejmuje wynik od 64 [również O(logn)].
        W paru ostatnich "blokach" kodu odpowiedzialnych za zliczanie liczby jedynek rezygnuje z maskowania szufladek
        korzystając z obserwacji, że od momentu w którym rozprawiamy się z szufladkami długości 8
        nie musimy przejmować się śmieciami powstałymi na skutek dodawania na starszych bitach 
        pod warunkiem, że wymaskujemy je na samym końcu (wynik zawsze zmieści się na 7 bitach)
 */

clz:
        # rozsmarowywanie bitów
        mov     %rdi, %rdx
        shr     %rdx 
        or      %rdx, %rdi 

        mov     %rdi, %rdx 
        shr     $2, %rdx 
        or      %rdx, %rdi

        mov     %rdi, %rdx 
        shr     $4, %rdx 
        or      %rdx, %rdi

        mov     %rdi, %rdx 
        shr     $8, %rdx 
        or      %rdx, %rdi

        mov     %rdi, %rdx 
        shr     $16, %rdx 
        or      %rdx, %rdi

        mov     %rdi, %rdx 
        shr     $32, %rdx 
        or      %rdx, %rdi

        # zliczanie zapalonych bitów
        mov     $0x5555555555555555, %r8
        mov     %rdi, %rdx
        shr     %rdx
        and     %r8, %rdi 
        and     %r8, %rdx 
        add     %rdx, %rdi

        mov     $0x3333333333333333, %r9
        mov     %rdi, %rdx
        shr     $2, %rdx
        and     %r9, %rdi 
        and     %r9, %rdx 
        add     %rdx, %rdi

        mov     $0x0f0f0f0f0f0f0f0f, %r10
        mov     %rdi, %rdx
        shr     $4, %rdx
        and     %r10, %rdi 
        and     %r10, %rdx 
        add     %rdx, %rdi

        mov     %rdi, %rdx
        shr     $8, %rdx
        add     %rdx, %rdi

        mov     %rdi, %rdx
        shr     $16, %rdx
        add     %rdx, %rdi

        mov     %rdi, %rdx
        shr     $32, %rdx
        add     %rdx, %rdi

        # wynik
        and     $0x7f, %rdi
        mov     $64, %rax
        sub     %rdi, %rax 
        ret
        
        .size   clz, .-clz
