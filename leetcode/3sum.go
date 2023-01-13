package main

import (
  "fmt"
  "sort"
)

func threeSum(nums []int) [][]int {
  sort.Ints(nums)
  sz := len(nums)

  i := 0
  j := sz / 2
  k := sz - 1

  s := nums[i] + nums[j] + nums[k]
  if s == 0 && sz == 3 {
    return [][]int{{i,j,k}}
  }

  r := [][]int{}
  for {
    s := nums[i] + nums[j] + nums[k]
    if s == 0 {
      r = append(r, []int{i, j, k})
    } else if s > 0 {
      for idx := k; idx > j; idx-- {
        ss := nums[i] + nums[j] + nums[idx]
        if ss < 0 {
          break
        }
        if ss == 0 {
          r = append(r, []int{i,j,idx})
        }
      }

      for idx := j; idx > i; idx-- {
        ss := nums[i] + nums[idx] + nums[k]
        if ss < 0 {
          break
        }
        if ss == 0 {
          r = append(r, []int{i,idx,k})
        }
      }
    } else if s < 0 {
    }
  }
  // return r
}

func main() {
  fmt.Println(threeSum([]int{0, 1, 1}))
  fmt.Println(threeSum([]int{0, 0, 0}))
}
