// Mateusz Łuczyński 331826
#include <iostream>
using namespace std;

// fast exponent of 7
long long int fast_pow(int exp) {
    if (exp == 0) return 1;
    long long int x = fast_pow(exp/2);
    return (exp % 2 ? x * x * 7 : x * x);
}

int main() {
    ios_base::sync_with_stdio(false); cin.tie(0); cout.tie(0);
    int w, k; cin >> w >> k;

    // init
    long long int** dp = new long long int* [3];
    long long int** board = new long long int* [3];
    for (int i = 0; i < 3; i++) {
        dp[i] = new long long int[k];
        board[i] = new long long int[k];
        string line; cin >> line;
        for (int j = 0; j < k; j++) {
            int exp = (int)line[j] - 48;
            board[i][j] = fast_pow(exp);
            dp[i][j] = 0;
        }
    }
    for (int j = 0; j < k; j++) dp[0][j] = board[0][j];

    // mainloop
    for (int i = 0; ; i++) {
        // update down
        for (int j = 0; j < k; j++) {
            // don't update if source unreachable
            if (dp[0][j] == 0) continue;
            if (j-1 >= 0) dp[2][j-1] = max(dp[2][j-1], dp[0][j] + board[2][j-1]);
            if (j+1 < k) dp[2][j+1] = max(dp[2][j+1], dp[0][j] + board[2][j+1]);
        }
        // update up
        for (int j = 0; j < k; j++) {
            // same here
            if (dp[2][j] == 0) continue;
            if (j-2 >= 0) dp[1][j-2] = max(dp[1][j-2], dp[2][j] + board[1][j-2]);
            if (j+2 < k) dp[1][j+2] = max(dp[1][j+2], dp[2][j] + board[1][j+2]);
        }
        cout << "step " << i << "\n";
        for (int y = 0; y < 3; y++) {
            for (int x = 0; x < k; x++) {
                cout << dp[y][x] << " ";
            }
            cout << "\n";
        }
        // check break-condition
        if (i + 3 == w) break;
        // shift
        delete [] dp[0]; delete [] board[0];
        dp[0] = dp[1]; dp[1] = dp[2];
        board[0] = board[1]; board[1] = board[2];
        dp[2] = new long long int[k]; board[2] = new long long int[k];
        // scan new line
        string line; cin >> line;
        for (int j = 0; j < k; j++) {
            int exp = (int)line[j] - 48;
            board[2][j] = fast_pow(exp);
            dp[2][j] = 0;
        }
    }

    // find result
    long long int res = -1;
    for (int j = 0; j < k; j++) {
        if (dp[2][j] > res) res = dp[2][j];
    }
    cout << res << "\n";
    
    // cleanup
    for (int i = 0; i < 3; i++) {
        delete [] dp[i]; delete [] board[i];
    }
    delete [] board; delete [] dp;

    return 0;
}