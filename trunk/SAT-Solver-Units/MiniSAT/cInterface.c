#define __STDC_LIMIT_MACROS
#define __STDC_FORMAT_MACROS

#include "cInterface.h"
#include <stdio.h>

TSolverID cCreateNewSolver ()
{
  return (CreateNewSolver ());

}

void cDeleteSolver (TSolverID ID)
{
  return DeleteSolver (ID);

}

int cGetNewVar (TSolverID ID, int Polarity, int Decide)
{
  return (GetNewVar (ID, Polarity, Decide));

}

int cSetRandomPolarity (TSolverID ID, int Flag)
{
  return (SetRandomPolarity (ID, Flag== 1));

}


int cAddClause (TSolverID ID, int Size)
{
  return (AddClause (ID, Size));

}

int cAddLiteralToClause (TSolverID ID, int Literal)
{
  return (AddLiteralToClause (ID, Literal));

}

int cSolve (TSolverID ID)
{
  return (Solve (ID));

}

int cSolve1 (TSolverID ID, int Literal)
{
  return (Solve1 (ID, Literal));

}


void cSetDecisionVar (TSolverID ID, int v, int b)
{
  return (SetDecisionVar (ID, v,  b));
}

int cGetValue (TSolverID ID, int v)
{
  return ((int) GetValue (ID, v));

}

int cGetValueInModel (TSolverID ID, int v)
{
  return ((int) GetValueInModel (ID, v));

}

int  cNoAssigns (TSolverID ID)
{
  return (NoAssigns (ID));

}

int  cNoClauses (TSolverID ID)
{
  return NoClauses (ID);

}

int  cNoVars (TSolverID ID)
{
  return (NoVars (ID));

}

int cIsSolverOkay (TSolverID ID)
{
  return (IsSolverOkay (ID));

}

int cFree ()
{
  return Free ();

}
