val additionalScalacOptions = Seq(
  "-target:jvm-1.8",
  "-encoding",
  "UTF-8",
  "-unchecked",
  "-deprecation",
  "-feature",
  "-language:existentials",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-language:postfixOps",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-infer-any",
  "-Ywarn-unused-import",
  "-Ypartial-unification",
  "-Xfatal-warnings",
  "-Xlint"
)

val projectSettings = Seq(
  name := "kademlia-sim",
  description := "Kademlia Simulation",
  version := "1.0",
  scalaVersion := "2.12.8",
  organization := "Sebastian Bach",
  scalacOptions ++= additionalScalacOptions,
  addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.9"),
  wartremoverExcluded += sourceManaged.value,
  wartremoverErrors in (Compile, compile) ++= Warts.allBut(
    // those we want
    Wart.DefaultArguments,
    Wart.ImplicitParameter,
    Wart.ImplicitConversion,
    Wart.LeakingSealed,
    Wart.Recursion,
    // those don't want
    Wart.Overloading,
    Wart.Nothing,
    Wart.Equals,
    Wart.PublicInference,
    Wart.TraversableOps,
    Wart.ArrayEquals,
    Wart.While,
    Wart.Any,
    Wart.Product,
    Wart.Serializable,
    Wart.OptionPartial,
    Wart.EitherProjectionPartial,
    Wart.Option2Iterable,
    Wart.ToString,
    Wart.JavaConversions,
    Wart.MutableDataStructures,
    Wart.FinalVal,
    Wart.Null,
    Wart.AsInstanceOf,
    Wart.ExplicitImplicitTypes,
    Wart.StringPlusAny,
    Wart.AnyVal
  ),
  scalafmtOnCompile := true
)

val dependencies = Seq(
  "org.jgrapht"        % "jgrapht-core"   % "1.3.0",
  "io.monix"          %% "monix"          % "3.0.0-RC2",
  "org.typelevel"     %% "cats-core"      % "1.6.0",
  "org.typelevel"     %% "cats-effect"    % "1.2.0",
  "org.typelevel"     %% "cats-mtl-core"  % "0.5.0",
  "com.typesafe.akka" %% "akka-actor"     % "2.5.21",
  "com.typesafe.akka" %% "akka-testkit"   % "2.5.21" % Test,
  "com.typesafe.akka" %% "akka-slf4j"     % "2.5.21",
  "ch.qos.logback"    % "logback-classic" % "1.2.3",
)

lazy val root = (project in file("."))
  .settings(projectSettings: _*)
  .settings(libraryDependencies ++= dependencies)
