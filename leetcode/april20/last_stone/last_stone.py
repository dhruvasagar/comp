def lastStoneWeight(stones):
    if len(stones) == 0:
        return 0
    if len(stones) == 1:
        return stones[0]

    stones.sort(reverse = True)
    if stones[0] == stones[1]:
        stones = stones[2:]
    else:
        stones[0] = stones[0] - stones[1]
        stones = stones[:1] + stones[2:]

    return lastStoneWeight(stones)

def main():
    print(lastStoneWeight([2,2]))
    print(lastStoneWeight([2, 7, 4, 1, 8, 1]))

if __name__ == "__main__":
    main()
