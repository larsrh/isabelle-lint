// DESCRIPTION: lint theory files

object Lint extends isabelle.Isabelle_Tool.Body
{

  import isabelle._

  lazy val options = Options.init()
  lazy val progress = new Console_Progress(verbose = true)

  def lint_dubious_imports(bases: Map[String, Sessions.Base]): Unit =
  {
    def dubious_imports(name: String, base: Sessions.Base): List[(String, String)] =
    {
      val graph = base.loaded_theories.dest
      for {
        ((thy, _), thy_deps) <- graph
        if thy.startsWith("HOL.")
        thy_dep <- thy_deps
        if !thy_dep.startsWith("HOL.") && thy_dep != "Main" && thy_dep != "Complex_Main"
      } yield ((thy_dep, thy))
    }

    val all_dubious_imports = bases.toList.flatMap { case (name, base) => dubious_imports(name, base) }.distinct

    if (!all_dubious_imports.isEmpty)
    {
      println("Found dubious imports:")
      for ((thy_dep, thy) <- all_dubious_imports)
        println(s"$thy_dep -> $thy")
    }
  }


  def lint(bases: Map[String, Sessions.Base]): Unit =
  {
    lint_dubious_imports(bases)
  }

  def apply(args: List[String]): Unit =
  {
    var select_dirs: List[Path] = Nil
    var exclude_session_groups: List[String] = Nil
    var all_sessions = false
    var dirs: List[Path] = Nil
    var session_groups: List[String] = Nil
    var exclude_sessions: List[String] = Nil

    val getopts = Getopts("""
Usage: isabelle lint [OPTIONS] [SESSIONS ...]

  Options are:
    -D DIR       include session directory and select its sessions
    -X NAME      exclude sessions from group NAME and all descendants
    -a           select all sessions
    -d DIR       include session directory
    -g NAME      select session group NAME
    -x NAME      exclude session NAME and all descendants

  Lint Isabelle theory files.

""",
      "D:" -> (arg => select_dirs = select_dirs ::: List(Path.explode(arg))),
      "X:" -> (arg => exclude_session_groups = exclude_session_groups ::: List(arg)),
      "a" -> (_ => all_sessions = true),
      "d:" -> (arg => dirs = dirs ::: List(Path.explode(arg))),
      "g:" -> (arg => session_groups = session_groups ::: List(arg)),
      "x:" -> (arg => exclude_sessions = exclude_sessions ::: List(arg)))

    val sessions = getopts(args)

    val structure = Sessions.load_structure(options, dirs, select_dirs)
    val selection = Sessions.Selection(
      all_sessions = all_sessions,
      exclude_session_groups = exclude_session_groups,
      exclude_sessions = exclude_sessions,
      session_groups = session_groups,
      sessions = sessions)
    val deps = structure.selection_deps(selection, progress = progress, verbose = true)

    lint(deps.session_bases)
  }

}
