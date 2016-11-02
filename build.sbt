import java.io.File
import sbt.Keys._
import sbt._

import spray.json._, DefaultJsonProtocol._

import gov.nasa.jpl.imce.sbt._
import gov.nasa.jpl.imce.sbt.ProjectHelper._

updateOptions := updateOptions.value.withCachedResolution(true)

resolvers := {
  val previous = resolvers.value
  if (git.gitUncommittedChanges.value)
    Seq[Resolver](Resolver.mavenLocal) ++ previous
  else
    previous
}

shellPrompt in ThisBuild := { state => Project.extract(state).currentRef.project + "> " }

lazy val core = Project("org-omg-oti-uml-json-serialization", file("."))
  .enablePlugins(IMCEGitPlugin)
  .enablePlugins(IMCEReleasePlugin)
  .settings(dynamicScriptsResourceSettings(Some("org.omg.oti.uml.json.serialization")))
  .settings(IMCEPlugin.strictScalacFatalWarningsSettings)
  .settings(IMCEReleasePlugin.packageReleaseProcessSettings)
  .settings(
    IMCEKeys.licenseYearOrRange := "2014-2016",
    IMCEKeys.organizationInfo := IMCEPlugin.Organizations.oti,
    IMCEKeys.targetJDK := IMCEKeys.jdk18.value,

    buildInfoPackage := "org.omg.oti.uml.json.serialization",
    buildInfoKeys ++= Seq[BuildInfoKey](BuildInfoKey.action("buildDateUTC") { buildUTCDate.value }),

    projectID := {
      val previous = projectID.value
      previous.extra(
        "build.date.utc" -> buildUTCDate.value,
        "artifact.kind" -> "generic.library")
    },

    git.baseVersion := Versions.version,

    unmanagedSourceDirectories in Compile +=
      baseDirectory.value / "src-gen",
    
    resourceDirectory in Compile :=
      baseDirectory.value / "resources",

    // disable publishing the jar produced by `test:package`
    publishArtifact in(Test, packageBin) := false,

    // disable publishing the test API jar
    publishArtifact in(Test, packageDoc) := false,

    // disable publishing the test sources jar
    publishArtifact in(Test, packageSrc) := false,

    unmanagedClasspath in Compile <++= unmanagedJars in Compile,

    extractArchives := {},

    resolvers += Resolver.bintrayRepo("jpl-imce", "gov.nasa.jpl.imce"),
    resolvers += Resolver.bintrayRepo("tiwg", "org.omg.tiwg")
  )
  .dependsOnSourceProjectOrLibraryArtifacts(
    "oti-uml-canonical_xmi-serialization",
    "org.omg.oti.uml.canonical_xmi.serialization",
    Seq(
      "org.omg.tiwg" %% "org.omg.oti.uml.canonical_xmi.serialization"
        % Versions_oti_uml_canonical_xmi_serialization.version % "compile" withSources() withJavadoc() artifacts
        Artifact("org.omg.oti.uml.canonical_xmi.serialization", "zip", "zip", Some("resource"), Seq(), None, Map())
    )
  )

def dynamicScriptsResourceSettings(dynamicScriptsProjectName: Option[String] = None): Seq[Setting[_]] = {

  import com.typesafe.sbt.packager.universal.UniversalPlugin.autoImport._

  def addIfExists(f: File, name: String): Seq[(File, String)] =
    if (!f.exists) Seq()
    else Seq((f, name))

  val QUALIFIED_NAME = "^[a-zA-Z][\\w_]*(\\.[a-zA-Z][\\w_]*)*$".r

  Seq(
    // the '*-resource.zip' archive will start from: 'dynamicScripts/<dynamicScriptsProjectName>'
    com.typesafe.sbt.packager.Keys.topLevelDirectory in Universal := {
      val projectName = dynamicScriptsProjectName.getOrElse(baseDirectory.value.getName)
      require(
        QUALIFIED_NAME.pattern.matcher(projectName).matches,
        s"The project name, '$projectName` is not a valid Java qualified name")
      Some(projectName)
    },

    // name the '*-resource.zip' in the same way as other artifacts
    com.typesafe.sbt.packager.Keys.packageName in Universal :=
      normalizedName.value + "_" + scalaBinaryVersion.value + "-" + version.value + "-resource",

    // contents of the '*-resource.zip' to be produced by 'universal:packageBin'
    mappings in Universal in packageBin ++= {
      val dir = baseDirectory.value
      val bin = (packageBin in Compile).value
      val src = (packageSrc in Compile).value
      val doc = (packageDoc in Compile).value
      val binT = (packageBin in Test).value
      val srcT = (packageSrc in Test).value
      val docT = (packageDoc in Test).value

      addIfExists(dir / ".classpath", ".classpath") ++
        addIfExists(dir / "README.md", "README.md") ++
        addIfExists(bin, "lib/" + bin.name) ++
        addIfExists(binT, "lib/" + binT.name) ++
        addIfExists(src, "lib.sources/" + src.name) ++
        addIfExists(srcT, "lib.sources/" + srcT.name) ++
        addIfExists(doc, "lib.javadoc/" + doc.name) ++
        addIfExists(docT, "lib.javadoc/" + docT.name)
    },

    artifacts += {
      val n = (name in Universal).value
      Artifact(n, "zip", "zip", Some("resource"), Seq(), None, Map())
    },
    packagedArtifacts += {
      val p = (packageBin in Universal).value
      val n = (name in Universal).value
      Artifact(n, "zip", "zip", Some("resource"), Seq(), None, Map()) -> p
    }
  )
}
