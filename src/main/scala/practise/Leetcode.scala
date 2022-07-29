package  practise

object Leetcode {
  
  /*
  给你一个整数 n，请你判断该整数是否是 2 的幂次方。如果是，返回 true ；否则，返回 false 。

  如果存在一个整数 x 使得 n == 2x ，则认为 n 是 2 的幂次方。

  来源：力扣（LeetCode）
  链接：https://leetcode.cn/problems/power-of-two
  著作权归领扣网络所有。商业转载请联系官方授权，非商业转载请注明出处。
  */
  def isPowerOfTwo(n: Int): Boolean = {
    if ((n & 0x1) == 1 && n > 1 || n <= 0) false
    else if(n == 1) true
    else isPowerOfTwo(n >> 1)
  }
}
