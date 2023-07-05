// Mateusz Łuczyński 331826
#include <iostream>
using namespace std;

int main() {
    ios_base::sync_with_stdio(false); cin.tie(0); cout.tie(0);
    int n; cin >> n;
    int * tab = new int[2 * n];
    long long int sum = 0;
    for (int i = 0; i < n; i++) {
        int a; cin >> a;
        tab[i] = a; tab[i + n] = a;
        sum += a;
    }
    int l = 0, r = 1; // [l, r)
    long long int current = tab[0];
    long long int res = -1;
    while (r <= 2 * n && l < n) {
        if (current <= sum - current) {
            if (current > res) res = current;
            current += tab[r];
            r++;
        } else {
            current -= tab[l];
            l++;
            if (l == r) {
                current = tab[l];
                r++;
            }
        }
    }
    cout << res << '\n';
    delete [] tab; 
    return 0;
}