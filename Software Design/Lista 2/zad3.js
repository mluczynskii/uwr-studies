console.log( (![]+[])[+[]]+(![]+[])[+!+[]]+([![]]+[][[]])[+!+[]+[+[]]]+(![]+[])[!+[]+!+[]] );
// evauluates to: 'fail'

console.log((![]+[])[+[]])
// ![] -> false (typeof [] == object => ![] == false)
// +[] -> 0 (casting [] to number)
// ![]+[] = 'false' (false.toString == 'false', [].toString == '')
// whole expression -> 'f' ('false'[0])


console.log((![]+[])[+!+[]])
// ![]+[] -> 'false'
// +!+[] -> +[] == 0, !+[] == true, +!+[] == 1
// whole expression -> 'a' ('false'[1])

console.log(([![]]+[][[]])[+!+[]+[+[]]])
// [![]] -> [false]
// [][[]] -> undefined (trying to access 1-st element of [])
// [![]]+[][[]] -> 'falseundefined' (toString on both and concat)
// [+[]] -> [0]
// +!+[]+[+[]] -> 1 + [0] = '1' + '0' = 10
// whole expression -> 'i' ('falseundefined'[10])

console.log((![]+[])[!+[]+!+[]])
// everything pretty much explained before
// whole expression -> 'l' ('false'[2])

