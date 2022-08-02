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

  class Node(var _value: Boolean, var _isLeaf: Boolean) {
    var value: Boolean = _value
    var isLeaf: Boolean = _isLeaf
    var topLeft: Node = null
    var topRight: Node = null
    var bottomLeft: Node = null
    var bottomRight: Node = null
  }

  // 构建四叉树
  def construct(grid: Array[Array[Int]]): Node = {


    def build(grid: Array[Array[Int]], row : Int, col : Int, size : Int) : Node = {
      if(size == 1) new Node(grid(row)(col) == 1, true)
      else {
        val subsize : Int = size / 2
        val topLeft: Node = build(grid, row, col, subsize)
        val topRight: Node = build(grid, row, col + subsize, subsize)
        val bottomLeft: Node = build(grid, row + subsize, col, subsize)
        val bottomRight: Node = build(grid, row + subsize, col + subsize, subsize)
        if(topLeft.isLeaf && topRight.isLeaf && bottomRight.isLeaf && bottomLeft.isLeaf &&
           topLeft.value == topRight.value && topLeft.value == bottomRight.value && topLeft.value == bottomLeft.value
        ) 
          new Node(topLeft.value, true)
        else {
          val newNode : Node = new Node(topLeft.value, false)
          newNode.topLeft = topLeft
          newNode.topRight = topRight
          newNode.bottomLeft = bottomLeft
          newNode.bottomRight = bottomRight
          newNode
        }
      }
    }
    build(grid, 0, 0, grid.length)
  }


  //   数字 n 代表生成括号的对数，请你设计一个函数，用于能够生成所有可能的并且 有效的 括号组合。

  //  

  //   示例 1：

  //   输入：n = 3
  //   输出：["((()))","(()())","(())()","()(())","()()()"]
  //   示例 2：

  //   输入：n = 1
  //   输出：["()"]
  //    

  //   提示：

  //   1 <= n <= 8

  //   来源：力扣（LeetCode）
  //   链接：https://leetcode.cn/problems/generate-parentheses
  //   著作权归领扣网络所有。商业转载请联系官方授权，非商业转载请注明出处。
  def generateParenthesis(n: Int): List[String] = {
  
    def dfs(str : String, left : Int, right :Int, cache : mutable.ListBuffer[String]) : Unit = {
      if(left == 0 && right == 0)
        cache.addOne(str)
      else {
        if(left <= right){
          if(left > 0) dfs(str + "(", left - 1, right, cache)
          if(right > 0) dfs(str + ")", left, right - 1, cache)
        }
      }
    }
    val ans = mutable.ListBuffer[String]()
    dfs("", n, n, ans)
    ans.toList
  }


  // 给定两个数组 nums1 和 nums2 ，返回 它们的交集 。输出结果中的每个元素一定是 唯一 的。我们可以 不考虑输出结果的顺序 。

  // 示例 1：

  // 输入：nums1 = [1,2,2,1], nums2 = [2,2]
  // 输出：[2]
  // 示例 2：

  // 输入：nums1 = [4,9,5], nums2 = [9,4,9,8,4]
  // 输出：[9,4]
  // 解释：[4,9] 也是可通过的

  // 来源：力扣（LeetCode）
  // 链接：https://leetcode.cn/problems/intersection-of-two-arrays
  // 著作权归领扣网络所有。商业转载请联系官方授权，非商业转载请注明出处。
  
  def intersection(nums1: Array[Int], nums2: Array[Int]): Array[Int] = {
    import scala.collection.mutable.HashSet
    import scala.collection.mutable.ArrayBuilder

    val builder : ArrayBuilder[Int] = ArrayBuilder.make
    val recorder : HashSet[Int] = HashSet()
    for(i <- nums1) recorder.add(i)
    for(i <- nums2 if recorder.contains(i)){
      builder.addOne(i)
      recorder.remove(i)
    }
    builder.result()
  }


  // 给定一个整数，写一个函数来判断它是否是 4 的幂次方。如果是，返回 true ；否则，返回 false 。

  // 整数 n 是 4 的幂次方需满足：存在整数 x 使得 n == 4x

  //  

  // 来源：力扣（LeetCode）
  // 链接：https://leetcode.cn/problems/power-of-four
  // 著作权归领扣网络所有。商业转载请联系官方授权，非商业转载请注明出处。
  def isPowerOfFour(n: Int): Boolean = {
    if(n == 1 || n == 4) true
    else if(n <= 0 || ((n & 0x3) > 0)) false
    else isPowerOfFour(n >> 2)
  }

  case class IntHolder(var value : Int);

  def longestPalindrome(s: String): Int = {
    import scala.collection.mutable.HashMap

    val chars : Array[Char] = s.toCharArray()
    val counts : HashMap[Char, IntHolder] = HashMap()
    for(el <- chars){
      val holder : IntHolder = counts.getOrElseUpdate(el, new IntHolder(0))
      holder.value += 1
    }
    var odd : Int = 0
    val sum = (for(count <- counts.values)
    yield {
      if (count.value % 2 == 1) odd = 1
      (count.value - count.value % 2)
    }).reduce((a, b) => a + b)
    sum + odd
  }
}
