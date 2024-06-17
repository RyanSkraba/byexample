package com.skraba.byexample.scalatags.duolingo.cn

case class Lessons(
    name: String,
    phrases: Vocab*
)

object Lessons {
  val Health2 = Lessons(
    "Health 2",
    Vocab("我今天生病了:Wo³ ji¹ntia¹n she³ngbi⁴ng le:I am sick today."),
    Vocab("我感冒了:Wo³ ga³nma⁴o le:I have a cold。"),
    Vocab("她下班了:Ta¹ xia³ba¹n le!:She got off work!")
  )

  val Vegetables = Lessons("Vegetables", Vocab("茄子:qie²zi:Eggplant"), Vocab("西红柿:xi¹hong²shi⁴:Tomato"))
}
