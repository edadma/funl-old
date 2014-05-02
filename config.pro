-injars			/home/ed/target/lower-thirds-editor/scala-2.10/LowerThirds.jar
-libraryjars	<java.home>/lib/rt.jar
-outjars		/home/ed/target/lower-thirds-editor/lte.jar

-dontwarn scala.**

-keepclasseswithmembers public class * {
    public static void main(java.lang.String[]);
}

-keepclassmembers class * {
    ** MODULE$;
}

-keep class * implements org.xml.sax.EntityResolver

-keepclassmembernames class scala.concurrent.forkjoin.ForkJoinPool {
  long ctl;
  long stealCount;
  int plock;
  int indexSeed;
}

-keepclassmembernames class scala.concurrent.forkjoin.ForkJoinPool$WorkQueue {
  int qlock;
}

-keepclassmembernames class scala.concurrent.forkjoin.ForkJoinTask {
  int status;
}