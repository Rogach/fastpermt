#include <map>
#include <utility>
#include <vector>
#include <stdlib.h>
#include <stdio.h>
#include <queue>
#include <cmath>

typedef std::map<int, std::vector<int>> graph;

extern "C" {
  float* fast_tfce(int inpc, float* inp, graph* gr) {

    float max = 0;
    for (int q = 0; q < inpc; q++)
      if (inp[q] > max)
        max = inp[q];

    float delta = max / 50;

    float* result = (float*) malloc(inpc * sizeof(float));
    for (int q = 0; q < inpc; q++) result[q] = 0;

    bool* visited = (bool*) malloc(inpc * sizeof(bool));
    for (int q = 0; q < inpc; q++) visited[q] = false;

    for (float t = delta/2; t < max; t += delta) {
      std::vector<int> current_cluster;

      for (int q = 0; q < inpc; q++) {
        if (!visited[q] && inp[q] > t) {
          std::queue<int> queue;
          queue.push(q);
          while (!queue.empty()) {
            int n = queue.front();
            queue.pop();
            if (!visited[n] && inp[n] > t) {
              current_cluster.push_back(n);
              std::vector<int> neighbors = (*gr)[n];
              for (uint i = 0; i < neighbors.size(); i++) {
                queue.push(neighbors[i]);
              }
              visited[n] = true;
            }
          }
          float add = pow(current_cluster.size(), 2.0f/3.0f) * pow(t, 2) * delta;
          for (uint i = 0; i < current_cluster.size(); i++) {
            result[current_cluster[i]] += add;
          }
          current_cluster.clear();
        }
      }
      for (int q = 0; q < inpc; q++) visited[q] = false;
    }

    free(visited);
    return result;
  }
}
