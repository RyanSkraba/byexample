package com.skraba.byexample.scalatags.duolingo.cn

/** A lesson is a collection of vocabulary words and/or phrases.
  *
  * @param name
  *   The name of the lesson.
  * @param phrases
  *   The vocabularies in the lesson.
  */
case class Lessons(
    name: String,
    phrases: Vocab*
)

object Lessons {
  val Health2: Lessons = Lessons(
    "Health 2",
    Vocab("我今天生病了:Wo³ ji¹ntia¹n she³ngbi⁴ng le:I am sick today."),
    Vocab("我感冒了:Wo³ ga³nma⁴o le:I have a cold。"),
    Vocab("她下班了:Ta¹ xia³ba¹n le!:She got off work!")
  )

  val FruitsAndVegetables: Lessons = Lessons(
    "Fruits and Vegetables",
    Vocab("苹果:pi²ngguo³:apple"),
    Vocab("香蕉:xia¹ngjia¹o:banana"),
    Vocab("花椰菜:hua¹ye²ca⁴i:broccoli"),
    Vocab("胡萝卜:hu²luo²bo:carrot"),
    Vocab("白菜:ba²ica⁴i:chinese cabbage"),
    Vocab("黄瓜:hua²nggua¹:cucumber"),
    Vocab("茄子:qie²zi:eggplant"),
    Vocab("大蒜:da⁴sua⁴n:garlic"),
    Vocab("葡萄:pu²tao:grape"),
    Vocab("青椒:qi¹ngjia¹o:green pepper"),
    Vocab("芒果:ma²ngguo³:mango"),
    Vocab("蘑菇:mo²gu:mushroom"),
    Vocab("洋葱:ya²ngco¹ng:onion"),
    Vocab("橙子:che²ngzi:orange"),
    Vocab("菠萝:bo¹luo²:pineapple"),
    Vocab("土豆:tu³do⁴u:potato"),
    Vocab("菠菜:bo¹ca⁴i:spinach"),
    Vocab("草莓:ca³ome²i:strawberry"),
    Vocab("西红柿:xi¹ho²ngshi⁴:tomato"),
    Vocab("西瓜:xi¹gua¹:watermelon")
  )
}
