package example

import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.classic.{Logger, LoggerContext}
import ch.qos.logback.core.Appender
import ch.qos.logback.core.read.ListAppender
import example.InspectionMacros.dumpExpression
import org.slf4j.LoggerFactory

import java.util.Objects.requireNonNull
import java.util.Optional

object Main {

  def main(args: Array[String]): Unit = {
    val logger = org.slf4j.LoggerFactory.getLogger("example.Main")
    InspectionMacros.decorateIfs(dif => logger.debug(s"${dif.code} = ${dif.result}")) {
      if (System.currentTimeMillis() - 1 < 0) {
        println("decorateIfs: if block")
      } else if (System.getProperty("derp") != null) {
        println("decorateIfs: if else block")
      } else {
        println("decorateIfs: else block")
      }
   }

    val string = java.time.Instant.now().toString
    InspectionMacros.decorateMatch(dm => logger.debug(s"${dm.code} = ${dm.result}")) {
      string match {
        case s if s.startsWith("20") =>
          println("this example is still valid")
        case _ =>
          println("oh dear")
      }
    }

    val dr: ExprInspection[Int] = InspectionMacros.dumpExpression(1 + 1)
    println(s"result: ${dr.code} = ${dr.value}")

    InspectionMacros.decorateVals(dval => logger.debug(s"${dval.name} = ${dval.value}")) {
       val a = 5
       val b = 15
       a + b
    }

    //val list = listAppender.list
    //println(list.get(0).getMessage) // check that the debug message is in there
  }

  //-----------------------------------------------
  // Logback chicanery follows

  def listAppender: ListAppender[ILoggingEvent] = {
    def getFirstAppender(logger: Logger): Optional[Appender[ILoggingEvent]] = {
      val appenderStream =
        StreamUtils.fromIterator(logger.iteratorForAppenders)
      appenderStream.findFirst
    }

    val loggerContext = LoggerFactory.getILoggerFactory.asInstanceOf[LoggerContext]
    val maybeAppender = getFirstAppender(loggerContext.getLogger(org.slf4j.Logger.ROOT_LOGGER_NAME))
    if (maybeAppender.isPresent)
      requireNonNull(maybeAppender.get).asInstanceOf[ListAppender[ILoggingEvent]]
    else throw new IllegalStateException("Cannot find appender")
  }

  object StreamUtils {
    import ch.qos.logback.core.Context
    import ch.qos.logback.core.spi.ContextAware
    import org.slf4j.Marker

    import java.util
    import java.util.Spliterators
    import java.util.stream.{Stream, StreamSupport}

    /**
     * Returns a stream containing the marker itself and the iterator belonging to the marker.
     *
     * @param marker the marker
     * @return empty stream if marker is null, stream.of(marker) if no references, otherwise
     *         Stream.of(marker, references)
     */
    def fromMarker(marker: Marker): Stream[Marker] = {
      if (marker == null) return Stream.empty
      if (!marker.hasReferences) return Stream.of(marker)
      Stream.concat(Stream.of(marker), fromIterator(marker.iterator))
    }

    def fromMarker(context: Context, marker: Marker): Stream[Marker] = fromMarker(marker).peek((s: Marker) => setContext(context, s))

    def fromIterator[E](iterator: util.Iterator[E]) = {
      val spliterator = Spliterators.spliteratorUnknownSize(iterator, 0)
      StreamSupport.stream(spliterator, false)
    }

    private def setContext[E](context: Context, s: E) = if (s.isInstanceOf[ContextAware]) s.asInstanceOf[ContextAware].setContext(context)
  }
}
