%{
  configs: [
    %{
      name: "default",
      files: %{
        included:
          case Mix.env() do
            :dev -> ["lib/", "src/", "web/", "apps/"]
            :test -> ["test/"]
          end,
        excluded: []
      },
      checks:
        [
          {Credo.Check.Consistency.ExceptionNames},
          {Credo.Check.Consistency.ParameterPatternMatching, false},
          {Credo.Check.Consistency.SpaceAroundOperators},
          {Credo.Check.Consistency.SpaceInParentheses},
          {Credo.Check.Consistency.TabsOrSpaces},
          {Credo.Check.Design.DuplicatedCode, false},
          {Credo.Check.Readability.MaxLineLength, max_length: 120},
          {Credo.Check.Readability.ParenthesesOnZeroArityDefs, false},
          {Credo.Check.Readability.PredicateFunctionNames},
          {Credo.Check.Readability.RedundantBlankLines, max_blank_lines: 2},
          {Credo.Check.Readability.Semicolons, false},
          {Credo.Check.Readability.TrailingBlankLine},
          {Credo.Check.Readability.TrailingWhiteSpace},
          {Credo.Check.Readability.VariableNames},
          {Credo.Check.Refactor.FunctionArity, false},
          {Credo.Check.Refactor.MapInto, false},
          {Credo.Check.Warning.IExPry},
          {Credo.Check.Warning.IoInspect},
          {Credo.Check.Warning.LazyLogging, false}
        ] ++
          case Mix.env() do
            :dev ->
              [
                {Credo.Check.Readability.ModuleDoc}
              ]

            :test ->
              []
          end
    }
  ]
}
