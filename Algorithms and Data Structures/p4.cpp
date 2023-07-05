#include <iostream>
#include <tuple>
#include <cstdlib>
using namespace std;

class Treap {
    private:
        struct Node {
            Node* left = nullptr; Node* right = nullptr;
            long long int value;
            int rank;
        };
        Node* root = nullptr;
        Node* merge (Node* u, Node* v) { // assert (data in u) < (data in v)
            if (u == nullptr) return v;
            else if (v == nullptr) return u;
            else if (u->rank > v->rank) {
                u->right = merge(u->right, v);
                return u;
            } else { 
                v->left = merge(u, v->left);
                return v;
            } 
        }
        typedef tuple<Node*, Node*, Node*> info;
        info split (Node* u, long long int x) {
            if (u == nullptr) return make_tuple(nullptr, nullptr, nullptr);
            else if (u->value == x) return make_tuple(u->left, u, u->right);
            else if (u->value < x ) {
                info res = split (u->right, x);
                u->right = get<0>(res);
                return make_tuple(u, get<1>(res), get<2>(res));
            } else {
                info res = split (u->left, x);
                u->left = get<2>(res);
                return make_tuple(get<0>(res), get<1>(res), u);
            }
        }
        Node* insert (Node* u, Node* entry) {
            info slices = split (u, entry->value);
            Node* v = merge (get<0>(slices), entry);
            return merge(v, get<2>(slices)); 
        }
        Node* remove (Node* u, long long int value) {
            info slices = split (u, value);
            return merge(get<0>(slices), get<2>(slices));
        }
        Node* find (Node* u, long long int value) {
            if (u == nullptr or u->value == value) return u;
            else if (u->value < value) return find(u->right, value);
            else return find(u->left, value); 
        }
        Node* upper (Node* u) {
            if (u == nullptr) return nullptr;
            while (u->left != nullptr)
                u = u->left;
            return u;
        }
        Node* lower (Node* u) {
            if (u == nullptr) return nullptr;
            while (u->right != nullptr)
                u = u->right;
            return u;
        }
        void destroy_aux (Node* u) {
            if (u == nullptr) return;
            destroy_aux(u->left);
            destroy_aux(u->right);
            delete u;
        }
    public:
        ~Treap() { destroy_aux (this->root); }
        void insert (long long int value) {
            if (find(this->root, value) != nullptr) return;
            Node* entry = new Node;
            entry->value = value;
            entry->rank = rand();
            this->root = this->insert(this->root, entry);
        }
        string remove (long long int value) {
            if (find(this->root, value) == nullptr) return "BRAK";
            this->root = this->remove(this->root, value);
            return "OK";
        }
        long long int upper (long long int value) {
            if (this->root == nullptr) { throw exception(); }; // upper-bound doesn't exist
            Node* u = this->root;
            long long int best = (u->value >= value ? u->value : __LONG_LONG_MAX__);
            bool flag = false;
            while(u != nullptr) { 
                if (u->value < value) {
                    u = u->right;
                } else {
                    if (!flag) best = u->value;
                    else best = min(best, u->value);
                    flag = true;
                    u = u->left;
                }
            }
            if (flag) return best;
            else throw exception();
        }   
        long long int lower (long long int value) {
            if (this->root == nullptr) { throw exception(); }; // lower-bound doesn't exist
            Node* u = this->root;
            long long int best;
            bool flag = false;
            while(u != nullptr) { 
                if (u->value > value) {
                    u = u->left;
                } else {
                    if (!flag) best = u->value;
                    else best = max(best, u->value);
                    flag = true;
                    u = u->right;
                }
            }
            if (flag) return best;
            else throw exception();
        }
};

int main() {
    ios_base::sync_with_stdio(false); cin.tie(0); cout.tie(0);
    srand((unsigned) time(nullptr));
    Treap tree;
    int n; cin >> n;
    while(n--) {
        char c; cin >> c;
        long long int value; cin >> value;
        string answer;
        long long int bound;
        switch(c) {
            case 'I':
                tree.insert(value);
                break;
            case 'D':
                answer = tree.remove(value);
                cout << answer << '\n';
                break;
            case 'U':
                try {
                    bound = tree.upper(value);
                    cout << bound << '\n';
                } catch (exception e) {
                    cout << "BRAK\n";
                }
                break;
            case 'L':
                try {
                    bound = tree.lower(value);
                    cout << bound << '\n';
                } catch (exception e) {
                    cout << "BRAK\n";
                }
                break;
        }
    }
    return 0;
}