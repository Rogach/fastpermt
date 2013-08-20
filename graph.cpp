#include <map>
#include <utility>
#include <vector>

typedef std::map<int, std::vector<int>> graph;

extern "C" {

  graph* create_graph() {
    return new graph();
  }

  void add_to_graph(graph* gr, int from, int toc, int* to) {
    std::vector<int> vto;
    vto.reserve(toc);
    for (int q = 0; q < toc; q++) {
      vto.push_back(to[q]);
    }
    (*gr).insert(std::pair<int, std::vector<int>>(from, vto));
  }

}
