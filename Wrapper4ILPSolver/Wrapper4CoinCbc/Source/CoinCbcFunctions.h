#ifndef GLPKFUNCTIONS_H
#define GLPKFUNCTIONS_H

#include "../GLPK-HeaderFiles/glpk.h"

glp_prob *GLPKCreateMaximizationProblem (char *Name);
glp_prob *GLPKCreateMinimizationProblem (char *Name);

void GLPKAddNewContinuesVariable (char *Name, int BoundStatus, double LowerBound, double UpperBound, double CoefInObjFunction, glp_prob *Handle);
void GLPKAddNewIntegerVariable (char *Name, int BoundStatus, double LowerBound, double UpperBound, double CoefInObjFunction, glp_prob *Handle);

void GLPKAddNewConstraint (char *RowName, int BoundStatus, double LowerBound, double UpperBound, glp_prob *Handle);

void GLPKLoadMatrix (int *ia, int *ib, int *ic, int ne, glp_prob *Handle);

void GLPKSimplex (glp_prob *Handle);
void GLPKIntOptimization (glp_prob *Handle);

double GLPKGetLPVariablePrimeValue (glp_prob *Handle, int VarIndex);
double GLPKGetMIPVariablePrimeValue (glp_prob *Handle, int VarIndex);

double GLPKGetLPObjectiveFunctionValue (glp_prob *Handle);

#endif
