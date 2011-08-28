#ifndef CINTERFACE_H
#define CINTERFACE_H
#ifdef __cplusplus

#include "Interface.h"
#include "core/SolverTypes.h"

#define EXPORTCALL __attribute__((stdcall))

typedef MiniSatSolversManager *TSolverManager;
typedef int TSolverID;
#else
typedef struct MiniSatSolversManager *TSolverManager;
typedef int TSolverID;
#define EXPORTCALL
#endif
#ifdef __cplusplus
extern "C"
{
#endif
    extern TSolverID EXPORTCALL CreateNewSolver ();
    extern void EXPORTCALL DeleteSolver (TSolverID);
    extern int EXPORTCALL GetNewVar (TSolverID, int PolarityMode, int Decide);
    extern int EXPORTCALL SetRandomPolarity (TSolverID, int);

    extern int EXPORTCALL AddClause (TSolverID, int);
    extern int EXPORTCALL AddLiteralToClause (TSolverID, int);

    extern int EXPORTCALL Solve (TSolverID);
    extern int EXPORTCALL Solve1 (TSolverID, int);
    extern void EXPORTCALL SetDecisionVar (TSolverID, int, int);
    extern int  EXPORTCALL GetValue (TSolverID, int);
    extern int  EXPORTCALL GetValueInModel (TSolverID, int);
    extern int  EXPORTCALL NoAssigns (TSolverID);
    extern int  EXPORTCALL NoClauses (TSolverID);
    extern int  EXPORTCALL NoVars (TSolverID);
    extern int  EXPORTCALL IsSolverOkay (TSolverID);
    extern int  EXPORTCALL Free ();

//
    extern TSolverID EXPORTCALL cCreateNewSolver ();
    extern void EXPORTCALL cDeleteSolver (TSolverID);
    extern int EXPORTCALL cGetNewVar (TSolverID, int, int);
    extern int EXPORTCALL cSetRandomPolarity (TSolverID, int);

    extern int EXPORTCALL cAddClause (TSolverID, int);
    extern int EXPORTCALL cAddLiteralToClause (TSolverID, int);

    extern int EXPORTCALL cSolve (TSolverID);
    extern int EXPORTCALL cSolve1 (TSolverID, int);
    extern void EXPORTCALL cSetDecisionVar (TSolverID, int , int);
    extern int EXPORTCALL cGetValue (TSolverID, int);
    extern int EXPORTCALL cGetValueInModel (TSolverID, int);
    extern int  EXPORTCALL cNoAssigns (TSolverID);
    extern int  EXPORTCALL cNoClauses (TSolverID);
    extern int  EXPORTCALL cNoVars (TSolverID);
    extern int  EXPORTCALL cIsSolverOkay (TSolverID);
    extern int  EXPORTCALL cFree ();

#ifdef __cplusplus
  }
#endif

#endif/*CINTERFACE_H*/

