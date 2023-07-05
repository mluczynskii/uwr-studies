#include <iostream>
#include <utility> // std::pair
#include <vector>
using namespace std;

class Heap {
    private:
        pair<int, long long int>* heap;
        int last = 1;
        int size;
        int* lookup;
        void swap(int k, int j) {
            if (k == j) return;
            // swap (key, priority)
            pair<int, long long int> t1 = this->heap[j];
            this->heap[j] = this->heap[k];
            this->heap[k] = t1;
            // swap positions in lookup table
            int t2 = this->lookup[this->heap[j].first];
            this->lookup[this->heap[j].first] = this->lookup[this->heap[k].first];
            this->lookup[this->heap[k].first] = t2;
        }
        void move_up(int index) {
            int k = index, j;
            do {
                j = k;
                if (j > 1 and this->heap[j/2] > this->heap[k])
                    k = j/2;
                this->swap(k, j);
            } while (j != k);
        }
        void move_down(int index) {
            int k = index, j;
            do {
                j = k;
                if (2*j <= this->last and this->heap[2*j] < this->heap[k])
                    k = 2*j;
                if (2*j < this->last and this->heap[2*j+1] < this->heap[k])
                    k = 2*j+1;
                this->swap(k, j);
            } while (j != k);
        }
    public:
        Heap(int n) : size(n) { 
            this->heap = new pair<int, long long int>[n+1]; // easier to index from 1
            this->lookup = new int[n+1];
            for (int i = 1; i <= n; i++) this->lookup[i] = -1;
        }
        ~Heap() { 
            delete [] this->heap; 
            delete [] this->lookup;
        }
        void push(int key, long long int priority) {
            if (last == size) throw runtime_error("the heap is full");
            int index = this->lookup[key];
            if (index != -1) {
                this->heap[index].second = min(this->heap[index].second, priority);
                this->move_up(index);
            } else {
                this->heap[this->last] = make_pair(key, priority);
                this->lookup[key] = this->last;
                this->move_up(index);
                this->last++;
            }
        }
        bool is_empty() { return this->last == 1; }
        pair<int, long long int> pop() {
            if (this->is_empty()) throw runtime_error("the heap is empty");
            pair<int, long long int> result = this->heap[1];
            this->lookup[result.first] = -1;
            this->last--;
            if (this->is_empty()) return result;
            this->heap[1] = this->heap[this->last];
            this->lookup[this->heap[1].first] = 1;
            this->move_down(1);
            return result;
        }
};

class Container {
    private:
        pair<int,int>* edge = nullptr;
        int count = 0;
    public:
        ~Container() { free(this->edge); }
        void push_back(pair<int,int> n_edge) {
            if (this->count == 0)
                this->edge = (pair<int,int>*)calloc(1, sizeof(pair<int,int>));
            else 
                this->edge = (pair<int,int>*)realloc(this->edge, sizeof(pair<int,int>)*this->count+1);
            this->edge[count] = n_edge;
            this->count++;
        }
        int get_count() { return this->count; }
        pair<int,int> get_elem(int index) {
            if (index >= count) throw runtime_error("index out of bounds");
            return this->edge[index];
        }
};


long long int* dijkstra(int n, Container* graph) {
    long long int* distance = new long long int[n+1];
    for (int i = 1; i <= n; i++)
        distance[i] = -1;
    distance[1] = 0;
    Heap q = Heap(n);
    q.push(1, 0);
    while (!q.is_empty()) {
        pair<int, long long int> v = q.pop();
        for (int i = 0; i < graph[v.first].get_count(); i++) {
            pair<int, int> u = graph[v.first].get_elem(i);
            if (distance[u.first] == -1 || distance[u.first] > u.second + distance[v.first]) {
                distance[u.first] = u.second + distance[v.first];
                q.push(u.first, distance[u.first]);
            }       
        }
    } 
    return distance;
}

int main() {
    ios_base::sync_with_stdio(false); cin.tie(0); cout.tie(0);
    int n, m, k; cin >> n >> m >> k;
    Container* graph = new Container[n+1];
    for (int i = 0; i < m; i++) {
        int a, b, weight; cin >> a >> b >> weight;
        graph[a].push_back(make_pair(b, weight));
        graph[b].push_back(make_pair(a, weight));
    }
    long long int* distance = dijkstra(n, graph);
    long long int result = 0;
    bool flag = true;
    for (int i = 0; i < k; i++) {
        int dest; cin >> dest;
        if (distance[dest] == -1) {
            flag = false;
            break;
        }
        result = result + distance[dest];
    }
    if (flag) cout << 2*result << "\n";
    else cout << "NIE\n";
    delete [] distance;
    delete [] graph;
    return 0;
}