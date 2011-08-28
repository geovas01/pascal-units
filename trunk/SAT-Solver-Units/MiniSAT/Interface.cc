#define __STDC_LIMIT_MACROS
#define __STDC_FORMAT_MACROS

#include <vector>
#include <iostream>

#include "Interface.h"
#include "core/Solver.h"

using namespace std;
using namespace Minisat;


Solver * MiniSatSolversManager::GetSolverByID (int SolverID)
{
  return Solvers [SolverID];

}

int MiniSatSolversManager::CreateNewSolver ()
{
  Solver *NewSolver= new Solver ();
  Solvers.push_back (NewSolver);
  vec<Lit> *EmptyClause= new vec<Lit> ();
  ActiveClauses.push_back (EmptyClause);
  NewSolver->rnd_pol= false;
 
  return (Solvers.size ()- 1);

}

int MiniSatSolversManager::AddLiteralToClause (int SolverID, int Literal)
{
  Lit l;
  l.SetX (Literal);
  ActiveClauses [SolverID]->push (l);

  return ActiveClauses [SolverID]->size ();

}

bool MiniSatSolversManager::DeleteSolver (int SolverID)
{
  if (Solvers [SolverID]== NULL)
    return false;
  else
  {
    delete GetSolverByID (SolverID);
    delete ActiveClauses [SolverID];
    Solvers [SolverID]= NULL;
    ActiveClauses [SolverID]= NULL;
    return true;

  }

}

Var MiniSatSolversManager::GetNewVar (int SolverID, int Polarity, bool Decide)
{
  if (Polarity== 2)
    return GetSolverByID (SolverID)->newVar (true, Decide);
  else
    return GetSolverByID (SolverID)->newVar (Polarity== 1, Decide);

}

int MiniSatSolversManager::SetRandomPolarity (int SolverID, bool Flag)
{
//  GetSolverByID (SolverID)->polarity_mode= Solver::polarity_rnd;

  return 0;

}


int MiniSatSolversManager::AddClause (int SolverID, int Size)
{
  int Result= 1;
  if (ActiveClauses [SolverID]->size ()== 0)
  {
    Lit l;
    l.SetX (2);
    ActiveClauses [SolverID]->push (l);

  }

  Result= GetSolverByID (SolverID)->addClause (*(ActiveClauses [SolverID]));
 
  ActiveClauses [SolverID]->clear ();

  return Result;
  
}

int MiniSatSolversManager::Solve (int SolverID)
{
  if (NoClauses (SolverID)== 0)
  {
    vec<Lit> Clause;
    Lit l;
    l.SetX (1);
    Clause.push (l);
    GetSolverByID (SolverID)->addClause (Clause);

  }

  GetSolverByID (SolverID)->verbosity= 0;
  if (!GetSolverByID (SolverID)->okay ())
    return false;
  int Result= GetSolverByID (SolverID)->solve ();
  return Result;

}

int MiniSatSolversManager::Solve (int SolverID, int Literal)
{
  if (NoClauses (SolverID)== 0)
  {
    vec<Lit> Clause;
    Lit l;
    l.SetX (1);
    Clause.push (l);
    GetSolverByID (SolverID)->addClause (Clause);

  }

  GetSolverByID (SolverID)->verbosity= 0;
  if (!GetSolverByID (SolverID)->okay ())
    return false;
  Lit lit;
  lit.SetX (Literal);
  int Result= GetSolverByID (SolverID)->solve (lit);
  return Result;

}


void MiniSatSolversManager::SetDecisionVar (int SolverID, Var V, bool b)
{
  GetSolverByID (SolverID)->setDecisionVar (V, b);

}

//This function returns a char (T, F or U). I had to change the declaration of this function
// becuase the values of l_True, l_False and l_Undef are changed in newer minisat.
int MiniSatSolversManager::GetValue (int SolverID, Var x)
{
  lbool V= GetSolverByID (SolverID)->value (x);

  if (V== l_True)
    return 'T';
  if (V== l_False)
    return 'F';
  if (V== l_Undef)
    return 'U';
  assert (false);

}

int MiniSatSolversManager::GetValueInModel (int SolverID, Var x)
{
  lbool V= GetSolverByID (SolverID)->modelValue (x);

  if (V== l_True)
    return 'T';
  if (V== l_False)
    return 'F';
  if (V== l_Undef)
    return 'U';
  assert (false);

}

int MiniSatSolversManager::NoAssigns (int SolverID)
{
  return GetSolverByID (SolverID)->nAssigns ();

}

int MiniSatSolversManager::NoClauses (int SolverID)
{
  return GetSolverByID (SolverID)->nClauses ();

}

int MiniSatSolversManager::NoVars (int SolverID)
{
  return GetSolverByID (SolverID)->nVars ();

}

void MiniSatSolversManager::Print ()
{
}

int MiniSatSolversManager::IsSolverOK (int SolverID)
{
  return GetSolverByID (SolverID)->okay ();

}

MiniSatSolversManager::~MiniSatSolversManager ()
{
  for (unsigned int i= 0; i< Solvers.size (); ++i)
    if (Solvers [i]!= NULL)
    {
      delete Solvers [i];
      delete ActiveClauses [i];

    }

  Solvers.resize (0);
  ActiveClauses.resize (0);

}
