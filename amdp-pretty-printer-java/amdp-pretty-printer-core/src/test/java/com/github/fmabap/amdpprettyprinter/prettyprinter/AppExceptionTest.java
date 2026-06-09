package com.github.fmabap.amdpprettyprinter.prettyprinter;

import org.junit.Test;
import static org.junit.Assert.*;

/**
 * Unit tests for {@link AppException}.
 */
public class AppExceptionTest {

    @Test
    public void messageConstructor_storesMessage() {
        // Arrange & Act
        AppException ex = new AppException("test message");

        // Assert
        assertEquals("test message", ex.getMessage());
    }

    @Test
    public void messageConstructor_extendsException() {
        // Arrange & Act
        AppException ex = new AppException("msg");

        // Assert
        assertTrue(ex instanceof Exception);
    }

    @Test
    public void messageCauseConstructor_storesMessageAndCause() {
        // Arrange
        Throwable cause = new RuntimeException("root cause");

        // Act
        AppException ex = new AppException("wrapper", cause);

        // Assert
        assertEquals("wrapper", ex.getMessage());
        assertSame(cause, ex.getCause());
    }

    @Test
    public void messageCauseConstructor_nullCause_storesNull() {
        // Arrange & Act
        AppException ex = new AppException("msg", null);

        // Assert
        assertEquals("msg", ex.getMessage());
        assertNull(ex.getCause());
    }
}
