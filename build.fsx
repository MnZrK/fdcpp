// include Fake libs
#r "./packages/FAKE/tools/FakeLib.dll"
#r "./packages/FSharpLint/FSharpLint.FAKE.dll"

open Fake
open Fake.Testing
open FSharpLint.FAKE

// Directories
let buildDir  = "./build/"
let deployDir = "./deploy/"

// Filesets
let appReferences  =
    !! "/**/*.csproj"
      ++ "/**/*.fsproj"
// TODO currently appReferences include testsReferences... may be we should remove them ? 

let testsReferences = 
    !! "**/*.Tests.fsproj"
      ++ "**/*.Tests.csproj"

// version info
let version = "0.1"  

// Targets
Target "Clean" (fun _ ->
    CleanDirs [buildDir; deployDir]
    !! "**/bin"
    ++ "**/obj"
    |> DeleteDirs
)

Target "Lint" (fun _ ->
    appReferences
        |> Seq.iter (FSharpLint id)
)

Target "Build" (fun _ ->
    MSBuildDebug buildDir "Build" appReferences
        |> Log "AppBuild-Output: "
)

Target "BuildRelease" (fun _ ->
    MSBuildRelease buildDir "Build" appReferences
        |> Log "AppBuild-Output: "
)

Target "BuildTests" (fun _ ->
    MSBuildDebug buildDir "Build" testsReferences
        |> Log "TestsBuild-Output: "
)

Target "RunTests" (fun _ ->
    !! (buildDir @@ "*.Tests.dll") 
    |> xUnit2 (fun p -> { p with Parallel = ParallelMode.All })
)

Target "RunTestsSequential" (fun _ ->
    !! (buildDir @@ "*.Tests.dll") 
    |> xUnit2 (fun p -> { p with Parallel = ParallelMode.NoParallelization })
)

Target "Deploy" (fun _ ->
    !! (buildDir + "/**/*.*")
        -- "*.zip"
        |> Zip buildDir (deployDir + "fundcpp." + version + ".zip")
)

// Build order
"Clean"
  ==> "Build"
  ==> "Deploy"

// Build order
"Clean"
  ==> "BuildRelease"
  ==> "Deploy"

// Build order
"Clean"
  ==> "BuildTests"
  ==> "RunTests"

// Build order
"Clean"
  ==> "BuildTests"
  ==> "RunTestsSequential"

// start build
RunTargetOrDefault "Build"
