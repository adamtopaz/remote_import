import Lean
import Lean.Util.Path
open Lean System

structure Package where
  path : FilePath

def Package.paths (P : Package) : IO LeanPaths := do
  let out ← IO.Process.output {
    cmd := "lake"
    args := #["print-paths"]
    cwd := P.path
  }
  let .ok out := Json.parse out.stdout | 
    throw <| .userError s!"Failed to parse paths:
{out.stdout}" 
  let .ok (paths : LeanPaths) := fromJson? out | 
    throw <| .userError s!"Failed to parse paths:
{out}"
  return {
    oleanPath := paths.oleanPath.map (P.path / ·)
    srcPath := paths.srcPath.map (P.path / ·)
    loadDynlibPaths := paths.loadDynlibPaths.map (P.path / ·)
  }

def Package.build (P : Package) : IO Unit := do
  let _ ← IO.Process.output {
    cmd := "lake"
    args := #["build"]
    cwd := P.path
    stderr := .piped
    stdout := .piped
  }

def Package.olean (P : Package) (module : Name) : IO FilePath := do
  P.build
  initSearchPath (← findSysroot) (← P.paths).oleanPath
  findOLean module

def Package.env (P : Package) (module : Name) : 
    IO (CompactedRegion × Environment) := do
  let mFile ← P.olean module 
  let (mod, region) ← readModuleData mFile
  let (_, s) ← importModulesCore mod.imports
    |>.run (s := { moduleNameSet := ({} : NameHashSet).insert module })
  let env ← finalizeImport s #[{module}] {} 0
  return (region, env)