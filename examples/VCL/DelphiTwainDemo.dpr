program DelphiTwainDemo;

uses
  Vcl.Forms,
  delphitwaindemo_form in 'delphitwaindemo_form.pas' {FormTwainDemo};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormTwainDemo, FormTwainDemo);
  Application.Run;
end.
