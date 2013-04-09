package org.scalatest.junit;

import org.junit.Test;
import org.junit.Ignore;

import static org.junit.Assert.*;

public class JBitterSuite {

  @Test public void verifySomething() {
    assertTrue(1 == 2); // This will fail
  }
}
