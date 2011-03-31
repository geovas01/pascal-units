unit LexerTokenTypeUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

type
  TModifierType= (tmtStart, tmtPrivate, tmtPublic, tmtProtected, tmtStatic, tmtFinal, tmtNative, tmtSynchronized,
  tmtAbstract, tmtThreadSafe, tmtTransient, tmtNone, tmtEnd);

const
  ModifierTypeString: array [tmtStart..tmtEnd] of String=
    ( '', 'private', 'public', 'protected', 'static', 'final', 'native', 'synchronized',
    'abstract', 'threadsafe', 'transient', '', '');

implementation

end.

