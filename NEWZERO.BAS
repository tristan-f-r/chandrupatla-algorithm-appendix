DECLARE SUB GETFUN (X#, F#, NFV%)
DEFINT I-N: DEFDBL A-H, O-Z
  CLS
  PRINT "*****      NEW ZERO FINDING ALGORITHM      *****"
  PRINT "*                T.R.CHANDRUPALTA              *"
  PRINT "************************************************"
  INPUT "INTERVAL A,B "; X1, X2
  EPS = 1E-10: DLT = .00001: NFG = 0
  CALL GETFUN(X1, F1, NFV)
  CALL GETFUN(X2, F2, NFV)
  T = .5
DO
  X = X1 + T * (X2 - X1)
  CALL GETFUN(X, F, NFV)
'---- Arrange 2-1-3:  2-1 Interval;  1 Middle;  3 Discarded Point ----
  IF SGN(F) = SGN(F1) THEN
    X3 = X1: X1 = X
    F3 = F1: F1 = F
  ELSE
    X3 = X2: X2 = X1: X1 = X1
    F3 = F2: F2 = F1: F1 = F
  END IF
'---- Identify the One that Approximates Zero ----'
  XM = X1: FM = F1
  IF ABS(F2) < ABS(F1) THEN
    XM = X2: FM = F2
  END IF
  TOL = 2 * EPS * ABS(XM) + .5 * DLT
  TL = TOL / ABS(X2 - X1)
  IF TL > .5 OR FM = 0 THEN EXIT DO
  '---- If Inverse Quadratic Interpolation Holds, Use it ----
  X1 = (X1 - X2) / (X3 - X2)
  PH = (F1 - F2) / (F3 - F2)
  FL = (1 - SQR(1 - X1)): FH = SQR(XI)
  IF FL < PH AND PH < FH THEN
    AL = (X3 - X1) / (X2 - X1)
    A = F1 / (F2 - F1): B = F3 / (F2 - F3)
    C = F1 / (F3 - F1): D = F2 / (F3 - F2)
    T + A * B + C * D * AL
  ELSE
    T = .5
  END IF
  '---- Adjust T Away from the Interval Boundary ----
  IF T < TL THEN T = TL
  IF T > 1 - TL THEN T = 1 - TL
LOOP
  PRINT "X = "; XM
  PRINT "F = "; FM
  PRINT "NFV = "; NFV
  END
SUB GETFUN(X, F< NFV)
NFV = NFV + 1
F = X * (X * X - 2) - 5
END SUB