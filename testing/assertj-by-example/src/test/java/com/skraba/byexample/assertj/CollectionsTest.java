package com.skraba.byexample.assertj;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.junit.jupiter.api.Test;

/** Tests and assertions for collection classes. */
class CollectionsTest {

  @Test
  void testArray() {
    String[] myArray = {"Apple", "Banana", "Carrot", "Dog"};
    assertThat(myArray)
        .hasSize(4)
        .hasSizeBetween(0, 5)
        .hasSizeGreaterThan(3)
        .contains("Apple")
        .containsAll(Arrays.asList("Apple", "Dog", "Banana"))
        .containsOnly("Apple", "Banana", "Carrot", "Dog");
  }

  @Test
  void testList() {
    List<String> myList = Arrays.asList("Apple", "Banana", "Carrot", "Dog");
    assertThat(myList)
        .hasSize(4)
        .hasSizeBetween(0, 5)
        .contains("Apple")
        .containsAll(Arrays.asList("Apple", "Dog", "Banana"))
        .containsOnly("Apple", "Banana", "Carrot", "Dog");
  }

  @Test
  void TestListFailsForNull() {
    assertThatThrownBy(() -> assertThat((List<String>) null).hasSize(4))
        .isInstanceOf(AssertionError.class)
        .hasMessageContaining("Expecting actual not to be null");
  }

  @Test
  void testSet() {
    Set<String> mySet = Stream.of("Apple", "Banana", "Carrot", "Dog").collect(Collectors.toSet());
    assertThat(mySet).contains("Apple");
  }

  @Test
  void testMap() {
    Map<String, Integer> myMap =
        Stream.of("Apple", "Banana", "Carrot", "Dog")
            .collect(Collectors.toMap(String::toLowerCase, String::length));
    assertThat(myMap).containsEntry("apple", 5);
  }
}
