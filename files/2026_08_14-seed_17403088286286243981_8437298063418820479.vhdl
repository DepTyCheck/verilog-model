-- Seed: 17403088286286243981,8437298063418820479

entity udzkg is
  port (sv : inout integer; xuuletgre : out severity_level; zpgvoidb : out time);
end udzkg;

architecture dygzkl of udzkg is
  
begin
  -- Single-driven assignments
  zpgvoidb <= zpgvoidb;
end dygzkl;

library ieee;
use ieee.std_logic_1164.all;

entity uzvbpxwob is
  port (bmwd : out real; qbspk : in std_logic_vector(3 to 2); l : buffer severity_level);
end uzvbpxwob;

architecture pgalya of uzvbpxwob is
  signal emuvp : time;
  signal fioxlrlh : integer;
  signal nuqdplapa : time;
  signal kbliveczxg : severity_level;
  signal qiultzp : integer;
begin
  lxmf : entity work.udzkg
    port map (sv => qiultzp, xuuletgre => kbliveczxg, zpgvoidb => nuqdplapa);
  aujwp : entity work.udzkg
    port map (sv => fioxlrlh, xuuletgre => l, zpgvoidb => emuvp);
  
  -- Single-driven assignments
  bmwd <= 0_4.0220;
end pgalya;

entity gtnnness is
  port (sjwwc : in time);
end gtnnness;

library ieee;
use ieee.std_logic_1164.all;

architecture zf of gtnnness is
  signal fuxkmap : time;
  signal ifjhbjltnj : severity_level;
  signal ndmagsj : integer;
  signal wdlm : severity_level;
  signal g : std_logic_vector(3 to 2);
  signal nhs : real;
begin
  cpznaj : entity work.uzvbpxwob
    port map (bmwd => nhs, qbspk => g, l => wdlm);
  d : entity work.udzkg
    port map (sv => ndmagsj, xuuletgre => ifjhbjltnj, zpgvoidb => fuxkmap);
  
  -- Multi-driven assignments
  g <= (others => '0');
  g <= g;
  g <= g;
end zf;



-- Seed after: 15515418181817228402,8437298063418820479
