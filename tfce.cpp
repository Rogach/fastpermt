#include <algorithm>
#include <cmath>
#include <functional>
#include <map>
#include <queue>
#include <stdio.h>
#include <stdlib.h>
#include <utility>
#include <vector>

typedef std::map<int, std::vector<int>> graph;
std::vector<std::vector<int>> get_clusters(int, float*, graph*, std::function<bool(float)>);

extern "C" {
  float* fast_tfce(int inpc, float* inp, graph* gr) {

    float* result = (float*) malloc(inpc * sizeof(float));
    for (int q = 0; q < inpc; q++) result[q] = 0;

    float max = *std::max_element(inp, inp+inpc);
    float posDelta = max / 50;

    for (float t = posDelta/2; t < max; t += posDelta) {
      auto clusters = get_clusters(inpc, inp, gr, [t](float v){ return v > t; });
      for (auto cluster : clusters) {
        float add = pow(cluster.size(), 2.0f/3.0f) * t*t * posDelta;
        for (int i : cluster) {
          result[i] += add;
        }
      }
    }

    float min = std::min_element(inp, inp+inpc);
    float negDelta = min / 50;

    for (float t = negDelta/2; t > min; t += negDelta) {
      auto clusters = get_clusters(inpc, inp, gr, [t](float v){ return v < t; });
      for (auto cluster : clusters) {
        float add = pow(cluster.size(), 2.0f/3.0f) * t*t * negDelta;
        for (int i : cluster) {
          result[i] += add;
        }
      }
    }

    return result;
  }

}

std::vector<std::vector<int>> get_clusters(int inpc, float* inp, graph* gr, std::function<bool(float)> test) {
  bool* visited = (bool*) malloc(inpc * sizeof(bool));
  for (int q = 0; q < inpc; q++) visited[q] = false;

  std::vector<std::vector<int>> clusters;
  for (int q = 0; q < inpc; q++) {
    if (!visited[q] && test(inp[q])) {
      std::vector<int> current_cluster;
      std::queue<int> queue;
      queue.push(q);
      while (!queue.empty()) {
        int n = queue.front();
        queue.pop();
        if (!visited[n] && test(inp[n])) {
          current_cluster.push_back(n);
          std::vector<int> neighbors = (*gr)[n];
          for (uint i = 0; i < neighbors.size(); i++) {
            queue.push(neighbors[i]);
          }
          visited[n] = true;
        }
      }
      clusters.push_back(current_cluster);
    }
  }
  free(visited);
  return clusters;
}
