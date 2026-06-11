-- Seed: 11638560352136752706,11181851762153539145

library ieee;
use ieee.std_logic_1164.all;

entity rnnpsfkgzu is
  port ( ewyhjjgli : in std_logic_vector(4 to 1)
  ; johwsddev : out std_logic_vector(4 downto 1)
  ; seinzuyn : linkage std_logic_vector(0 to 1)
  ; zsfadaxk : inout time
  );
end rnnpsfkgzu;



architecture lygdxadgx of rnnpsfkgzu is
  
begin
  
end lygdxadgx;

library ieee;
use ieee.std_logic_1164.all;

entity pploy is
  port (rl : in std_logic; tcro : in severity_level; jjxj : inout real; zg : in integer);
end pploy;



architecture irii of pploy is
  
begin
  
end irii;



entity h is
  port (uujexl : linkage time);
end h;

library ieee;
use ieee.std_logic_1164.all;

architecture shxso of h is
  signal ieyuj : integer;
  signal vdon : real;
  signal sjmcnt : severity_level;
  signal srmpux : std_logic;
begin
  nrxep : entity work.pploy
    port map (rl => srmpux, tcro => sjmcnt, jjxj => vdon, zg => ieyuj);
end shxso;



entity vwfwqiwj is
  port (gc : out time);
end vwfwqiwj;

library ieee;
use ieee.std_logic_1164.all;

architecture kvikd of vwfwqiwj is
  signal wfwbdmub : time;
  signal adl : time;
  signal e : std_logic_vector(4 to 1);
  signal rk : time;
  signal ejjbofpenz : std_logic_vector(0 to 1);
  signal qyw : std_logic_vector(4 downto 1);
  signal nqvpxz : std_logic_vector(4 to 1);
begin
  yfoodddoz : entity work.h
    port map (uujexl => gc);
  xldraqmtkg : entity work.rnnpsfkgzu
    port map (ewyhjjgli => nqvpxz, johwsddev => qyw, seinzuyn => ejjbofpenz, zsfadaxk => rk);
  rtr : entity work.rnnpsfkgzu
    port map (ewyhjjgli => e, johwsddev => qyw, seinzuyn => ejjbofpenz, zsfadaxk => adl);
  wu : entity work.h
    port map (uujexl => wfwbdmub);
end kvikd;



-- Seed after: 11332697427460389280,11181851762153539145
