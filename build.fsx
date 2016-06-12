// include Fake libs
#r "./packages/FAKE/tools/FakeLib.dll"

open Fake
open Fake.Testing

// Directories
let buildDir  = "./build/"
let deployDir = "./deploy/"

// Filesets
let appReferences  =
    !! "/**/*.csproj"
      ++ "/**/*.fsproj"

let testsReferences = 
    !! "**/*.Tests.fsproj"
      ++ "**/*.Tests.csproj"

// version info
let version = "0.1"  // or retrieve from CI server

// Targets
Target "Clean" (fun _ ->
    CleanDirs [buildDir; deployDir]
)

Target "Build" (fun _ ->
    MSBuildDebug buildDir "Build" appReferences
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

// start build
RunTargetOrDefault "Build"
