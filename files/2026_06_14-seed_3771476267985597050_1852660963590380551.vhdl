-- Seed: 3771476267985597050,1852660963590380551



entity mwsfi is
  port (xc : inout boolean; nepadjh : in boolean_vector(1 to 4); xhhyk : out real);
end mwsfi;



architecture jpb of mwsfi is
  
begin
  
end jpb;



entity xqg is
  port (dazxeelygr : buffer integer; bzrhfyl : inout time);
end xqg;



architecture dicoefek of xqg is
  
begin
  
end dicoefek;



entity ukcrtcejqp is
  port (vuptvge : inout boolean; duxlicitbi : buffer character; nutaiidm : linkage time; yslsrodmkp : inout integer);
end ukcrtcejqp;



architecture nveaa of ukcrtcejqp is
  signal cdmca : real;
  signal vtamtbp : boolean_vector(1 to 4);
begin
  psdntbdt : entity work.mwsfi
    port map (xc => vuptvge, nepadjh => vtamtbp, xhhyk => cdmca);
end nveaa;

library ieee;
use ieee.std_logic_1164.all;

entity y is
  port (imbkhkmz : linkage std_logic; ocqovdrc : out time);
end y;



architecture bzjsrv of y is
  signal pytnmnbkf : integer;
  signal rfalusuc : character;
  signal mvhju : boolean;
  signal afwdxvrpgp : time;
  signal cq : integer;
begin
  lzrlw : entity work.xqg
    port map (dazxeelygr => cq, bzrhfyl => afwdxvrpgp);
  a : entity work.ukcrtcejqp
    port map (vuptvge => mvhju, duxlicitbi => rfalusuc, nutaiidm => ocqovdrc, yslsrodmkp => pytnmnbkf);
end bzjsrv;



-- Seed after: 14286625997708224876,1852660963590380551
