package com.skraba.byexample.scala;

/** An example of a bean-like class in Java. */
public class GreeterBean {

  private int count;
  private String name;

  public GreeterBean() {}

  public GreeterBean(int count, String name) {
    this.count = count;
    this.name = name;
  }

  public int getCount() {
    return count;
  }

  public void setCount(int count) {
    this.count = count;
  }

  public String getName() {
    return name;
  }

  public void setName(String name) {
    this.name = name;
  }
}
