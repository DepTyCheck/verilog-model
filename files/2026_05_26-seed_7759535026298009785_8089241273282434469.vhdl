-- Seed: 7759535026298009785,8089241273282434469



entity jqlwm is
  port (wtvgyfqs : in time_vector(0 to 4); z : buffer integer; yuympmjkxm : in time);
end jqlwm;



architecture i of jqlwm is
  
begin
  
end i;

library ieee;
use ieee.std_logic_1164.all;

entity dkrwogxcxr is
  port (vjiqszhgzx : out std_logic_vector(4 downto 1); fktiav : linkage integer);
end dkrwogxcxr;



architecture igtwhmw of dkrwogxcxr is
  signal ewayiuqbg : time;
  signal fjpkw : integer;
  signal kuf : time_vector(0 to 4);
  signal ajpfscw : time;
  signal jlzwzbkw : integer;
  signal gbfntuhlxz : time_vector(0 to 4);
  signal e : time;
  signal qje : integer;
  signal dpdqvctshp : time_vector(0 to 4);
begin
  foc : entity work.jqlwm
    port map (wtvgyfqs => dpdqvctshp, z => qje, yuympmjkxm => e);
  gpjacrcdu : entity work.jqlwm
    port map (wtvgyfqs => gbfntuhlxz, z => jlzwzbkw, yuympmjkxm => ajpfscw);
  xhytn : entity work.jqlwm
    port map (wtvgyfqs => kuf, z => fjpkw, yuympmjkxm => ewayiuqbg);
end igtwhmw;



entity zxpuesvpk is
  port (tjodpej : linkage integer_vector(3 to 1));
end zxpuesvpk;



architecture auqbmv of zxpuesvpk is
  
begin
  
end auqbmv;



entity adnvxicyu is
  port (panxchv : in bit);
end adnvxicyu;

library ieee;
use ieee.std_logic_1164.all;

architecture znl of adnvxicyu is
  signal r : integer_vector(3 to 1);
  signal fecas : time;
  signal ztkp : integer;
  signal tz : time_vector(0 to 4);
  signal xqnm : std_logic_vector(4 downto 1);
  signal cfgb : integer;
  signal nk : std_logic_vector(4 downto 1);
begin
  iyeklgfk : entity work.dkrwogxcxr
    port map (vjiqszhgzx => nk, fktiav => cfgb);
  yxhe : entity work.dkrwogxcxr
    port map (vjiqszhgzx => xqnm, fktiav => cfgb);
  edzzyline : entity work.jqlwm
    port map (wtvgyfqs => tz, z => ztkp, yuympmjkxm => fecas);
  cqije : entity work.zxpuesvpk
    port map (tjodpej => r);
end znl;



-- Seed after: 8498478868138016614,8089241273282434469
