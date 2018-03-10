#pragma once

#include <functional>
#include <vector>

// AET = Array Element Type
// VRT = Valuator Return Type
template <typename AET,
          typename VRT>
int pqsPartition(std::vector<AET>& array,
                 unsigned int p, unsigned int r,
                 const std::function<VRT(AET)>& valuator) {
    int i = p - 1,
        j = r + 1;
    const VRT x = valuator(array[p]);

    while (true) {
        do {
            j--;
        } while (valuator(array[j]) > x);

        do {
            i++;
        } while (valuator(array[i]) < x);

        if (i < j)
            std::swap(array[i], array[j]);
        else
            return j;
    }
}

/* destructively-sorts the array */
template <typename AET,
          typename VRT>
std::vector<AET>& pQuicksort(std::vector<AET>& array,
                             unsigned int lo, unsigned int hi,
                             const std::function<VRT(AET)>& valuator) {
    if (lo < hi) {
        const int p = pqsPartition<AET, VRT>(array, lo, hi, valuator);
        pQuicksort<AET, VRT>(array, lo, p, valuator);
        pQuicksort<AET, VRT>(array, p + 1, hi, valuator);
    }

    return array;
}
