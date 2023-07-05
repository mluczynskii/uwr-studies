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
H: .quad 0x8080808080808080
L: .quad 0x7f7f7f7f7f7f7f7f

        .text
        .globl  addsb
        .type   addsb, @function

/*
 * W moim rozwiązaniu używam następującej techniki: 
        Dodawanie wektorów (L2.4) + sprytne maskowanie, które trochę ciężko wytłumaczyć,
        bo jest efektem zapisania dziesięciu tysięcy kartek losowymi operacjami bitowymi (see komentarze przy kodzie)
        Ogólna idea jest taka, że tworzymy maskę, która najpierw zeruje elementy wektora, w których
        wystąpiło przepełnienie, następnie wpisujemy w te elementy odpowiednio wartość minimalną i maksymalną.
 */

addsb:
        /* normal vector addition (without saturation) */
        movq    %rdi, %r10 
        movq    %rsi, %r11
        andq    H, %r10 
        andq    H, %r11 

        andq    L, %rdi 
        andq    L, %rsi 

        leaq    (%rdi, %rsi), %rax
        xorq    %r10, %rax 
        xorq    %r11, %rax

        /* fixing saturation */
        movq    %rax, %r8 
        andq    H, %r8
        movq    %r8, %rdx /* save sig(res) */

        xorq    %r10, %r8 
        xorq    H, %r8     /* 0x0 where sig(x) != sig(res), 0x80 otherwise */
        xorq    %r10, %r11 /* 0x0 where sig(x) = sig(y), 0x80 otherwise */
        orq     %r11, %r8  /* 0x0 where over/underflow happened, 0x80 otherwise */

        shrq    $7, %r8 
        leaq    (%r8, %r8, 2), %r8 
        leaq    (%r8, %r8, 4), %r8 
        leaq    (, %r8, 8), %r9 
        leaq    (%r8, %r9, 2), %r8 /* 0x0 -> over/underflow, 0xff otherwise */
        
        /* sig(res) = 1 -> overflow, sig(res) = 0 -> underflow */
        andq    %r8, %rax /* clear blocks where over/underflow happened */
        notq    %r8 /* 0xff -> over/underflow, 0x0 otherwise */

        movq    %rdx, %r9
        orq     L, %r9 
        notq    %r9 
        movq    %r8, %r10 
        andq    %r10, %r9      
        orq     %r9, %rax /* good blocks unchanged, underflow blocks = 0x80, overflow blocks unchanged */

        movq    H, %r9
        shrq    $7, %rdx
        subq    %rdx, %r9 
        andq    %r9, %r8 
        andq    L, %r8  
        orq     %r8, %rax /* good blocks unchanged, underflow blocks unchanged, overflow blocks = 0x7f */

        ret /* done */

        .size   addsb, .-addsb
