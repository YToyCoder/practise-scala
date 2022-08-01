package  practise

import scala.collection.mutable

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


  /*

  如果字符串 s 中 不存在 两个不同字符 频次 相同的情况，就称 s 是 优质字符串 。

  给你一个字符串 s，返回使 s 成为 优质字符串 需要删除的 最小 字符数。

  字符串中字符的 频次 是该字符在字符串中的出现次数。例如，在字符串 "aab" 中，'a' 的频次是 2，而 'b' 的频次是 1 。

  来源：力扣（LeetCode）
  链接：https://leetcode.cn/problems/minimum-deletions-to-make-character-frequencies-unique
  著作权归领扣网络所有。商业转载请联系官方授权，非商业转载请注明出处。
  */
  def minDeletions(s: String): Int = {
    import scala.collection.mutable
    val chars : Array[Char] = s.toCharArray()
    val counts : Array[Int] = new Array[Int](26)
    for(c <- chars;
        index : Int = c - 'a'
    ) 
      counts(index) += 1
    val fill : mutable.Set[Int] = mutable.Set()
    var ans = 0
    for(count <- counts){
      var n : Int = count
      while(fill.contains(n)){
        n -= 1
        ans += 1
      }
      if(n != 0) fill.add(n)
    }
    ans
  }

  /*

  给定一个字符串 s ，你需要反转字符串中每个单词的字符顺序，同时仍保留空格和单词的初始顺序。

  */
  def reverseWords(s: String): String = {
    val resultBuilder = mutable.StringBuilder.newBuilder
    val chars : Array[Char] = s.toCharArray();
    var start : Int = 0
    for(i <- 0 until chars.length){
      if(i == chars.length - 1 || chars(i + 1) == ' '){
        for(j <- (i to start by -1)){
          resultBuilder.append(chars(j))
        }
        // end reset start
        start = i + 2
      }else if(chars(i) == ' ')resultBuilder.append(chars(i))
    }
    resultBuilder.toString()
  }


  /*
  给你一个整数 n，请你返回一个含 n 个字符的字符串，其中每种字符在该字符串中都恰好出现 奇数次 。

  返回的字符串必须只含小写英文字母。如果存在多个满足题目要求的字符串，则返回其中任意一个即可。

  来源：力扣（LeetCode）
  链接：https://leetcode.cn/problems/generate-a-string-with-characters-that-have-odd-counts
  著作权归领扣网络所有。商业转载请联系官方授权，非商业转载请注明出处。
  */
  def generateTheString(n: Int): String = {
    def repeat(c : Char, n : Int): String = (0 until n).map(e => c).mkString 
    if(n % 2 == 0) repeat('q',(n - 1)) + "a"
    else repeat('q',n)  
  }


}
