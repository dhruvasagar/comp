package main

import "fmt"

func median(nums []int) float64 {
  n := len(nums)
  if n % 2 == 0 {
    return (float64(nums[(n/2) - 1]) + float64(nums[n/2])) / 2
  }

  return float64(nums[n / 2])
}

func findMedianSortedArrays(nums1 []int, nums2 []int) float64 {
  m := len(nums1)
  n := len(nums2)
  nums := []int{}
  i := 0
  j := 0
  for i + j < m + n {
    if i == m {
      nums = append(nums, nums2[j])
      j++
    } else if j == n {
      nums = append(nums, nums1[i])
      i++
    } else {
      if nums1[i] <= nums2[j] {
        nums = append(nums, nums1[i])
        i++
      } else {
        nums = append(nums, nums2[j])
        j++
      }
    }
  }
  return median(nums)
}

func main() {
  fmt.Println(findMedianSortedArrays([]int{1, 3}, []int{2}))
}
