package com.skraba.byexample.json.base;

import org.junit.jupiter.api.Test;

/** Base class to demonstrate how to do things in Java */
public interface JsonTestBase {

  @Test
  void testParseStringIntoJson();

  @Test
  void testParseStreamIntoJson();
}
