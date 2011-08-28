#define __STDC_LIMIT_MACROS
#define __STDC_FORMAT_MACROS
#include "cInterface.h"
#include <iostream>

MiniSatSolversManager *Manager= new MiniSatSolversManager ();

TSolverID CreateNewSolver ()
{
  return Manager->CreateNewSolver ();

}

void DeleteSolver (TSolverID ID)
{
  Manager->DeleteSolver (ID);

}

int GetNewVar (TSolverID ID, int Polarity, int Decide)
{
  return Manager->GetNewVar (ID, Polarity, Decide== 1);

}

int SetRandomPolarity (TSolverID ID, int Flag)
{
  return Manager->SetRandomPolarity (ID, Flag);

}


int AddClause (TSolverID ID, int Size)
{
  return Manager->AddClause (ID, Size);

}

int AddLiteralToClause (TSolverID ID, int Literal)
{
  return Manager->AddLiteralToClause (ID, Literal);

}

int Solve (TSolverID ID)
{
  return Manager->Solve (ID);

}

int Solve1 (TSolverID ID, int Literal)
{
  return Manager->Solve (ID, Literal);

}

void SetDecisionVar (TSolverID ID, Var v, int b)
{
  return Manager->SetDecisionVar (ID, v,  b);
}

int GetValue (TSolverID ID, Var v)
{
  return ((int) Manager->GetValue (ID, v));

}

int GetValueInModel (TSolverID ID, Var v)
{
  return ((int) Manager->GetValueInModel (ID, v));

}


int NoAssigns (TSolverID ID)
{
  return Manager->NoAssigns (ID);

}

int NoClauses (TSolverID ID)
{
  return Manager->NoClauses (ID);

}

int NoVars (TSolverID ID)
{
  return Manager->NoVars (ID);

}

int IsSolverOkay (TSolverID ID)
{
  return Manager->IsSolverOK (ID);

}

int Free ()
{
  delete Manager;

  return 0;

}
