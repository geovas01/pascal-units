#ifndef INTERFACE_H
#define INTERFACE_H

#include <assert.h>

#include "mtl/Vec.h"
#include "core/SolverTypes.h"
#include "core/Solver.h"
#include<vector>

#ifndef Minisat_Vec_h
 adfdsdf
#endif

using namespace std;
using namespace Minisat;

class MiniSatSolversManager
{
private:
  vector<vec<Lit> *> ActiveClauses;
  vector<Solver *> Solvers;

public:
void Print ();
/*Creates a new MiniSatSolver and returns an ID (integer)*/
int CreateNewSolver ();
bool DeleteSolver (int SolverID);
Var GetNewVar (int SolverID, int Polarity, bool Decide);
int SetRandomPolarity (int SolverID, bool PolarityMode);
//bool AddClauses (int SolverID, Lit *Clause);
int AddClause (int SolverID, int Size);
int Solve (int SolverID);
int Solve (int SolverID, int Literal);
void SetDecisionVar (int SolverID, Var V, bool b);
int GetValue (int SolverID, Var x);
int GetValueInModel (int SolverID, Var x);
int NoAssigns (int SolverID);
int NoClauses (int SolverID);
int NoVars (int SolverID);
int AddLiteralToClause (int SolverID, int Literal);
int IsSolverOK (int SolverID);

//void GetAssigns (int SolverID, char *Assignments);
//
Solver *GetSolverByID (int ID);

~MiniSatSolversManager ();

};

#endif
