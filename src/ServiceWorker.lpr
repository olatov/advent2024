program Serviceworker;

{$mode objfpc}

uses
  Classes, Serviceworkerapp;

const
  Yourcachename = 'v1'; // usually increased with every version
    // The cache is specific to your domain, so no need to include your app name.

type

  { TApplication }

  Tapplication = class(Tserviceworkerapplication)
  Public
    constructor Create(Aowner: Tcomponent); Override;
  end;

{ TApplication }

constructor Tapplication.Create(Aowner: Tcomponent);
begin
  inherited Create(Aowner);

  Fcachename := Yourcachename;
  Fresources := ['images/error.png'];
  Fallbackurl := 'images/error.png';
end;

var
  App: Tapplication;
begin
  App := Tapplication.Create(nil);
  App.Run;
end.
