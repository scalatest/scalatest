package org.scalatest.junit;

import junit.framework.TestCase;

public class JUnit3TestCase extends TestCase {

    public JUnit3TestCase(String name) {
        super(name);
    }

    public void testA() {
        assertEquals('a', 'a');
    }

    public void testB() {
        assertEquals('b', 'b');
    }

    public void testC() {
        assertEquals('c', 'b');
    }
}
