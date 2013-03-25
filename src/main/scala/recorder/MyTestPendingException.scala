package recorder

class MyTestPendingException(val message: String, val context: String, val cause: Throwable, val fileNameAndLineNumber: Option[String]) extends Exception(message, cause) {}

class MyTestFailedException(val message: String, val context: String, val cause: Throwable, val fileNameAndLineNumber: Option[String]) extends Exception(message, cause) {}

class MyException(val message: String, val context: String, val cause: Throwable, val fileNameAndLineNumber: Option[String]) extends Exception(message, cause) {}
