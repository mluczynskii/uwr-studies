// only one per file, doesn't have to be named
export default function() {
    console.log("Hello world!");
}

// multiple named exports
export const pi = 3.14;
export function square(n: number) {
    return n*n;
}