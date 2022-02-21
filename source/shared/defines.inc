// Application Definitions for FPC & Lazarus

{$mode objfpc}{$H+}

{$if defined(windows)}
     {$define Windows}
{$elseif defined(darwin)}
     {$define MacOS}
{$elseif defined(linux)}
     {$define Linux}
{$else}
     {$define Unknown}
{$ifend}

