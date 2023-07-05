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
m0:     .quad   0xaaaaaaaaaaaaaaaa /* 10101010... */
m1:     .quad   0xcccccccccccccccc /* 11001100... */
m2:     .quad   0xf0f0f0f0f0f0f0f0 /* 11110000... */
m3:     .quad   0xff00ff00ff00ff00 /* 1111111100000000... */
m4:     .quad   0xffff0000ffff0000 /* etc. */
m5:     .quad   0xffffffff00000000 

        .text
        .globl  wbs
        .type wbs, @function

/*
 * W moim rozwiązaniu używam następującej techniki: 
        Korzystamy silnie z własności rozdzielności mnożenia względem dodawania oraz
        faktu, że każdą liczbę możemy przedstawić w postaci sumy potęg dwójki (system binarny).
        Idea na przykładzie 8 bitów:
        liczbę 7 w systemie binarnym możemy zapisać jako 2^2 + 2^1 + 2^0
        6 = 2^2 + 2^1
        5 = 2^2 + 2^0
        ...
        1 = 2^0
        0 = 0 (duh)
        Wyrażenie b_7 * 7 zamienia się wtedy w b_7 * 2^2 + b_7 * 2^1 + b_7 * 2^0.
        Rozpisując sumę z zadania według wzoru wyżej i wyciągając potęgi 2 przed nawiasy otrzymujemy
        wynik = 2^0(b_1 + b_3 + b_5 + b_7) + 2^1(b_2 + b_3 + b_6 + b_7) + 2^0(b_4 + b_5 + b_6 + b_7),
        a to już zdecydowanie łatwiej policzyć (i to w pożądanej złożoności czasowej).
        Dla słów 64 bitowych wystarczy ustalić które bity mają stać przy odpowiednich potęgach 2
        (od 2^0 do 2^5), zmaskować je, zsumować klasycznym algorytmem popcount i przemnożyć przez 2^k.
 */

wbs:
        /* 2^0 first */
        movq    %rdi, %r8
        andq    m0, %r8
        popcnt  %r8, %rax

        /* 2^1 */
        movq    %rdi, %r9
        andq    m1, %r9 
        popcnt  %r9, %r9
        leaq    (%rax, %r9, 2), %rax

        /* 2^2 */
        movq    %rdi, %r10
        andq    m2, %r10 
        popcnt  %r10, %r10
        leaq    (%rax, %r10, 4), %rax

        /* 2^3 */
        movq    %rdi, %r11
        andq    m3, %r11 
        popcnt  %r11, %r11
        leaq    (%rax, %r11, 8), %rax

        /* 2^4 */
        movq    %rdi, %r8
        andq    m4, %r8 
        popcnt  %r8, %r8
        shlq    $4, %r8 
        addq    %r8, %rax 

        /* 2^5 */
        movq    %rdi, %r9
        andq    m5, %r9 
        popcnt  %r9, %r9
        shlq    $5, %r9 
        addq    %r9, %rax 

        ret

        .size wbs, .-wbs
