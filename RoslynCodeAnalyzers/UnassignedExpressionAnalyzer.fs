namespace ReqexAnalyzerFs

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.Diagnostics
open System.Collections.Immutable
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.CSharp.Syntax

[<DiagnosticAnalyzer(LanguageNames.CSharp)>]
type UnassignedExpressionAnalyzer() = 
    inherit DiagnosticAnalyzer()

    let rule = DiagnosticDescriptor("UnassignedExpressionAnalyzer",
                                    "Result of an expression should be assigned to a variable or used as an argument.",
                                    "A result of the expression of type '{0}' is never used.",
                                    "Semantic", 
                                    DiagnosticSeverity.Warning, 
                                    true, 
                                    "Result of an expression should be assigned to a variable or passed as an argument.", "")
    
    let toFullyQualifiedName (ts:ITypeSymbol) =
        let format = SymbolDisplayFormat(typeQualificationStyle = SymbolDisplayTypeQualificationStyle.NameAndContainingTypesAndNamespaces)
        ts.ToDisplayString(format)

    let analyzeInvocationExpression (context:SyntaxNodeAnalysisContext) =

        // guaranteed by node action registration
        let invocationExpr = context.Node :?> InvocationExpressionSyntax

        if invocationExpr.Parent :? ExpressionStatementSyntax then
            match context.SemanticModel.GetTypeInfo(invocationExpr).ConvertedType |> Option.ofObj with
            | Some expressionType when expressionType.SpecialType <> SpecialType.System_Void -> 
                 Diagnostic.Create(rule, invocationExpr.GetLocation(), toFullyQualifiedName expressionType)
                 |> context.ReportDiagnostic

            | _ -> ()

    override __.SupportedDiagnostics
        with get() = ImmutableArray.Create(rule)
    
    override __.Initialize context =
        context.RegisterSyntaxNodeAction(analyzeInvocationExpression, SyntaxKind.InvocationExpression)

