package org.scalatest.junit;

import junit.framework.TestSuite;
import junit.framework.Test;

public class JUnit3TestSuite extends TestSuite {

    public JUnit3TestSuite(String name) {
        super(name);
    }

    public static Test suite() {
        TestSuite suite = new TestSuite();

        suite.addTest(new JUnit3TestCase("testA"));
        suite.addTest(new JUnit3TestCase("testB"));

        return suite;
    }
}
