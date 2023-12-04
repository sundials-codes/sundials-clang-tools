#include <clang/AST/ASTConsumer.h>
#include <clang/AST/RecursiveASTVisitor.h>
#include <clang/Frontend/CompilerInstance.h>
#include <clang/Rewrite/Core/Rewriter.h>
#include <clang/Tooling/CommonOptionsParser.h>
#include <clang/Tooling/Refactoring.h>
#include <clang/Tooling/Tooling.h>
#include <fstream>
#include <iostream>
#include <llvm/Support/raw_ostream.h>
#include <memory>
#include <sstream>
#include <string>

using namespace clang;
using namespace clang::tooling;

class FindProcessErrorVisitor
    : public RecursiveASTVisitor<FindProcessErrorVisitor> {
public:
  explicit FindProcessErrorVisitor(ASTContext &Context, Rewriter &R,
                                     bool rewrite)
      : TheRewriter(R), Context(Context), rewrite(rewrite) {}

  bool VisitCallExpr(CallExpr *E) {

    FunctionDecl *callee = E->getDirectCallee();
    if (!callee) {
      return true; // Ignore this function call
    }

    if (callee->getNameInfo().getAsString() == "cvProcessError" ||
        callee->getNameInfo().getAsString() == "IDAProcessError" ||
        callee->getNameInfo().getAsString() == "arkProcessError" ||
        callee->getNameInfo().getAsString() == "KINProcessError") {
      std::string newCall;
      llvm::raw_string_ostream rso(newCall);

      // Write the callee.
      rso << Lexer::getSourceText(
          CharSourceRange::getTokenRange(E->getCallee()->getSourceRange()),
          TheRewriter.getSourceMgr(), TheRewriter.getLangOpts());

      // Write the argument list.
      rso << "(";
      rso << Lexer::getSourceText(
          CharSourceRange::getTokenRange(E->getArg(0)->getSourceRange()),
          TheRewriter.getSourceMgr(), TheRewriter.getLangOpts());
      rso << ", "
          << Lexer::getSourceText(
                 CharSourceRange::getTokenRange(E->getArg(1)->getSourceRange()),
                 TheRewriter.getSourceMgr(), TheRewriter.getLangOpts());
      rso << ", __LINE__";
      rso << ", __func__";
      rso << ", __FILE__";
      rso << ", "
          << Lexer::getSourceText(
                 CharSourceRange::getTokenRange(E->getArg(4)->getSourceRange()),
                 TheRewriter.getSourceMgr(), TheRewriter.getLangOpts());
      rso << ")";

      if (!rewrite) {
        auto &Diagnostics = Context.getDiagnostics();
        const auto ReplacePoint = E->getBeginLoc();
        const auto ID =
            Diagnostics.getCustomDiagID(clang::DiagnosticsEngine::Warning,
                                        "method '%0' should be replaced");
        clang::DiagnosticBuilder Diagnostic =
            Diagnostics.Report(ReplacePoint, ID);
        Diagnostic.AddString(callee->getNameInfo().getAsString());

        const auto FixIt =
            clang::FixItHint::CreateReplacement(ReplacePoint, "this");
        Diagnostic.AddFixItHint(FixIt);
      } else {
        TheRewriter.ReplaceText(E->getSourceRange(), rso.str());
      }
    }

    return true;
  }

private:
  Rewriter &TheRewriter;
  ASTContext &Context;
  bool rewrite;
};

class FindProcessErrorConsumer : public clang::ASTConsumer {
public:
  explicit FindProcessErrorConsumer(ASTContext *Context, Rewriter &R,
                                      bool rewrite)
      : Visitor(*Context, R, rewrite) {}

  virtual void HandleTranslationUnit(clang::ASTContext &Context) {
    Visitor.TraverseDecl(Context.getTranslationUnitDecl());
  }

private:
  FindProcessErrorVisitor Visitor;
};

class FindProcessErrorAction : public clang::ASTFrontendAction {
public:
  FindProcessErrorAction(bool rewrite) : rewrite(rewrite) {}

  virtual std::unique_ptr<clang::ASTConsumer>
  CreateASTConsumer(clang::CompilerInstance &Compiler,
                    llvm::StringRef InFile) override {
    TheRewriter.setSourceMgr(Compiler.getSourceManager(),
                             Compiler.getLangOpts());
    return std::make_unique<FindProcessErrorConsumer>(
        &Compiler.getASTContext(), TheRewriter, rewrite);
  }

  bool BeginSourceFileAction(clang::CompilerInstance &Compiler) override {
    llvm::errs() << "Processing " << getCurrentFile() << "\n\n";
    return true;
  }

  void EndSourceFileAction() override {
    if (!rewrite)
      return;
    std::error_code EC;
    llvm::raw_fd_ostream Out(
        TheRewriter.getSourceMgr()
            .getFileEntryForID(TheRewriter.getSourceMgr().getMainFileID())
            ->getName(),
        EC, llvm::sys::fs::OF_None);
    TheRewriter.getEditBuffer(TheRewriter.getSourceMgr().getMainFileID())
        .write(Out);
  }

private:
  Rewriter TheRewriter;
  bool rewrite;
};

std::unique_ptr<FrontendActionFactory>
myNewFrontendActionFactory(bool rewrite) {

  class MyFrontendActionFactory : public FrontendActionFactory {
  public:
    MyFrontendActionFactory(bool rewrite) : rewrite(rewrite) {}

    std::unique_ptr<FrontendAction> create() override {
      return std::make_unique<FindProcessErrorAction>(rewrite);
    }

  private:
    bool rewrite;
  };

  return std::unique_ptr<FrontendActionFactory>(
      new MyFrontendActionFactory(rewrite));
}

int main(int argc, const char **argv) {
  using namespace clang::tooling;

  // Parse the command-line options.
  llvm::cl::OptionCategory MyToolCategory("my-tool options");
  llvm::cl::opt<bool> rewrite(
      "rewrite", llvm::cl::desc("apply fixes to source code files in place"),
      llvm::cl::cat(MyToolCategory));
  auto ExpectedOptionsParser =
      CommonOptionsParser::create(argc, argv, MyToolCategory);
  if (!ExpectedOptionsParser) {
    llvm::errs() << ExpectedOptionsParser.takeError();
    return 1;
  }
  CommonOptionsParser &OptionsParser = ExpectedOptionsParser.get();

  // Create a new Clang Tool instance (a LibTooling frontend).
  ClangTool Tool(OptionsParser.getCompilations(),
                 OptionsParser.getSourcePathList());

  // Run the Clang Tool with a new FrontendAction (created by the factory).
  Tool.run(myNewFrontendActionFactory(rewrite).get());

  return 0;
}
