package com.skraba.byexample.assertj;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

import java.util.*;
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
  void testIterator() {
    assertThat(Stream.of("x").iterator()).hasNext();
    assertThat(Stream.empty().iterator()).isExhausted();
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
    assertThatThrownBy(() -> assertThat(new MightBeNull(true).get()).hasSize(4))
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
    assertThat(myMap)
        .containsEntry("apple", 5)
        .containsKey("apple")
        .containsValue(5)
        .hasEntrySatisfying("apple", v -> assertThat(v).isEqualTo(5));
  }

  /** A little helper so that we can test a null instance of a String list knowing it will fail. */
  static class MightBeNull {
    private final boolean isNull;

    MightBeNull(boolean isNull) {
      this.isNull = isNull;
    }

    List<String> get() {
      return isNull ? null : new ArrayList<>();
    }
  }
}
