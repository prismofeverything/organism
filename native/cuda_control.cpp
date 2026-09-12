#include <ATen/Context.h>
#include <c10/cuda/CUDACachingAllocator.h>
extern "C" int organism_cuda_fraction(double fraction) {
  try { at::globalContext().lazyInitDevice(at::kCUDA); c10::cuda::CUDACachingAllocator::setMemoryFraction(fraction, 0); return 0; }
  catch (...) { return -1; }
}
extern "C" void organism_cuda_empty_cache() { c10::cuda::CUDACachingAllocator::emptyCache(); }
