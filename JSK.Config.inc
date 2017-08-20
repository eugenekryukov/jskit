{.$DEFINE BENCH}

{.$DEFINE DXDEBUG}

{$MINENUMSIZE 4}
{$ALIGN ON}
{$BOOLEVAL OFF}
{$LONGSTRINGS ON}
{$WRITEABLECONST ON}
{$SCOPEDENUMS ON}

{$B-} {- Complete Boolean Evaluation }
{$X+} {- Extended syntax }

{$IFDEF MSWINDOWS}
  {$DEFINE WINDOWS}
  {$DEFINE VCL}
{$ENDIF}

{$IFDEF OSX}
{$ENDIF}

{$IF DEFINED(IOSSIMULATOR) or DEFINED(IOS) or DEFINED(ANDROID)}
  Unsupported platform
{$IFEND}
