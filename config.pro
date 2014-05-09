-injars			/home/ed/target/funl/scala-2.10/funl.jar
#-injars			/home/ed/target/lia/scala-2.10/lia_2.10-0.1-SNAPSHOT.jar
#-injars			/home/ed/target/indentation-lexical/scala-2.10/indentation-lexical_2.10-0.1-SNAPSHOT.jar
-libraryjars	<java.home>/lib/rt.jar
-outjars		/home/ed/target/funl/funl-proguard.jar

-dontwarn scala.**

-keepclasseswithmembers public class * {
    public static void main(java.lang.String[]);
}

-keepclassmembers class * {
    ** MODULE$;
}

-keep class * implements org.xml.sax.EntityResolver

# -keepclassmembernames class scala.concurrent.forkjoin.ForkJoinPool {
#   long ctl;
#   long stealCount;
#   int plock;
#   int indexSeed;
# }
# 
# -keepclassmembernames class scala.concurrent.forkjoin.ForkJoinPool$WorkQueue {
#   int qlock;
# }
# 
# -keepclassmembernames class scala.concurrent.forkjoin.ForkJoinTask {
#   int status;
# }