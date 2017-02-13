scalaVersion := "2.12.1"

val wrongDirs = Set("project","target","default")

def isModuleDir(file : File) : Boolean = {
        file.isDirectory && !file.isHidden && !wrongDirs(file.getName)
}

def subDirectories(path : String) = new File(path)
  .listFiles.filter( isModuleDir).map(_.getName)

lazy val subProjects = subDirectories(".").map(name =>ProjectRef(file(name), name))

lazy val root = Project("root", file(".")).aggregate(subProjects : _ *)








