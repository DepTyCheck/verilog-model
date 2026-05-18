-- Seed: 7737019969334144442,18238119570016518405

library ieee;
use ieee.std_logic_1164.all;

entity uw is
  port (df : out time; gzwg : in time; w : in std_logic; hgvzko : inout time_vector(2 downto 3));
end uw;



architecture r of uw is
  
begin
  
end r;

library ieee;
use ieee.std_logic_1164.all;

entity bjy is
  port (ledjxqxp : in std_logic; hd : in std_logic_vector(3 to 0); uflo : inout real);
end bjy;



architecture bsop of bjy is
  signal tb : time_vector(2 downto 3);
  signal bluy : time;
  signal pzxji : time;
begin
  ri : entity work.uw
    port map (df => pzxji, gzwg => bluy, w => ledjxqxp, hgvzko => tb);
end bsop;

library ieee;
use ieee.std_logic_1164.all;

entity oarjvuyt is
  port (jlr : buffer std_logic; hi : inout boolean_vector(3 downto 0); mphokkhr : in std_logic);
end oarjvuyt;

library ieee;
use ieee.std_logic_1164.all;

architecture zcgz of oarjvuyt is
  signal atdmu : time_vector(2 downto 3);
  signal adepec : std_logic;
  signal gsdxwmkbhi : time;
  signal qo : time;
begin
  jmrimj : entity work.uw
    port map (df => qo, gzwg => gsdxwmkbhi, w => adepec, hgvzko => atdmu);
end zcgz;

library ieee;
use ieee.std_logic_1164.all;

entity breikmpe is
  port (nlodc : linkage std_logic; nw : buffer time; z : inout real);
end breikmpe;

library ieee;
use ieee.std_logic_1164.all;

architecture nlcpmx of breikmpe is
  signal hjprtr : std_logic;
  signal bypfcc : boolean_vector(3 downto 0);
  signal xl : std_logic;
  signal lj : time_vector(2 downto 3);
  signal izpoyfpy : time_vector(2 downto 3);
  signal nws : time;
  signal ynrip : time_vector(2 downto 3);
  signal irkyyn : std_logic;
  signal say : time;
begin
  mmqlbbqyr : entity work.uw
    port map (df => say, gzwg => nw, w => irkyyn, hgvzko => ynrip);
  ymylfl : entity work.uw
    port map (df => nw, gzwg => nws, w => irkyyn, hgvzko => izpoyfpy);
  p : entity work.uw
    port map (df => nws, gzwg => nw, w => irkyyn, hgvzko => lj);
  iqrhqxom : entity work.oarjvuyt
    port map (jlr => xl, hi => bypfcc, mphokkhr => hjprtr);
end nlcpmx;



-- Seed after: 11011014051618997901,18238119570016518405
