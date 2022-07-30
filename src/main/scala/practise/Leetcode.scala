package  practise

object Leetcode {
  
  /*
  给你一个整数 n，请你判断该整数是否是 2 的幂次方。如果是，返回 true ；否则，返回 false 。

  如果存在一个整数 x 使得 n == 2x ，则认为 n 是 2 的幂次方。

  来源：力扣（LeetCode）
  链接：https://leetcode.cn/problems/power-of-two
  著作权归领扣网络所有。商业转载请联系官方授权，非商业转载请注明出处。
  */
  def isPowerOfTwo(n: Int): Boolean = n > 0 && (n & (n - 1)) == 0 


  /*

  给你一个由 不同 整数组成的整数数组 arr 和一个整数 k 。

  每回合游戏都在数组的前两个元素（即 arr[0] 和 arr[1] ）之间进行。比较 arr[0] 与 arr[1] 的大小，较大的整数将会取得这一回合的胜利并保留在位置 0 ，较小的整数移至数组的末尾。当一个整数赢得 k 个连续回合时，游戏结束，该整数就是比赛的 赢家 。

  返回赢得比赛的整数。

  题目数据 保证 游戏存在赢家。

  来源：力扣（LeetCode）
  链接：https://leetcode.cn/problems/find-the-winner-of-an-array-game
  著作权归领扣网络所有。商业转载请联系官方授权，非商业转载请注明出处。
  */
  def getWinner(arr: Array[Int], k: Int): Int ={

    def find(arr: Array[Int], k : Int, count : Int, max : Int, next : Int) : Int = {
      if(next >= arr.length) max
      else if(count == k) max
      else {
        val keep = arr(next) < max
        find(arr, k, if(keep) count + 1 else 1, if(keep) max else arr(next), next + 1)
      }
    }

    find(arr, k, 1, Math.max(arr(0), arr(1)), 2)
  } 


  /*
  圣诞活动预热开始啦，汉堡店推出了全新的汉堡套餐。为了避免浪费原料，请你帮他们制定合适的制作计划。

  给你两个整数 tomatoSlices 和 cheeseSlices，分别表示番茄片和奶酪片的数目。不同汉堡的原料搭配如下：

  巨无霸汉堡：4 片番茄和 1 片奶酪
  小皇堡：2 片番茄和 1 片奶酪
  请你以 [total_jumbo, total_small]（[巨无霸汉堡总数，小皇堡总数]）的格式返回恰当的制作方案，使得剩下的番茄片 tomatoSlices 和奶酪片 cheeseSlices 的数量都是 0。

  如果无法使剩下的番茄片 tomatoSlices 和奶酪片 cheeseSlices 的数量为 0，就请返回 []。

  来源：力扣（LeetCode）
  链接：https://leetcode.cn/problems/number-of-burgers-with-no-waste-of-ingredients
  著作权归领扣网络所有。商业转载请联系官方授权，非商业转载请注明出处。
  */
  def numOfBurgers(tomatoSlices: Int, cheeseSlices: Int): List[Int] = {
    if(tomatoSlices % 2 == 1 || tomatoSlices < cheeseSlices * 2 || cheeseSlices * 4 < tomatoSlices)
      List.empty
    else List(tomatoSlices / 2 - cheeseSlices, cheeseSlices * 2 - tomatoSlices / 2)
  }

}
