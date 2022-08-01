### practise scala 

### 1 （self type) 自身类型

从技术上讲,自身类型是在类中提到this时,对于this的假定类型。从实用角度讲,自身类型指定了对于特质能够混入的具体类的要求。如果你的特质仅能用于混入另一个或几个特定的特质,那么你可以指定那些特质作为自身类型。

```scala
trait TraitType {
  this : SelfType => 
}
```