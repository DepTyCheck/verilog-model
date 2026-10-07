-- Seed: 4300916410492035978,5906004015519833893

library ieee;
use ieee.std_logic_1164.all;

entity eekvuemr is
  port (suaen : buffer std_logic_vector(4 downto 2); jutdapscib : in time_vector(1 to 2); bty : out real; t : inout time_vector(1 downto 1));
end eekvuemr;

architecture ucqqqof of eekvuemr is
  
begin
  -- Multi-driven assignments
  suaen <= suaen;
end ucqqqof;

library ieee;
use ieee.std_logic_1164.all;

entity awmjqgb is
  port (sx : inout std_logic_vector(2 to 4); aoltjzsvk : out time; mahmticye : out real);
end awmjqgb;

architecture ibj of awmjqgb is
  signal zlo : time_vector(1 downto 1);
  signal mjgz : time_vector(1 to 2);
  signal sq : time_vector(1 downto 1);
  signal fljo : real;
  signal kwuedwpiu : time_vector(1 to 2);
begin
  taxqnrhtjy : entity work.eekvuemr
    port map (suaen => sx, jutdapscib => kwuedwpiu, bty => fljo, t => sq);
  ncab : entity work.eekvuemr
    port map (suaen => sx, jutdapscib => mjgz, bty => mahmticye, t => zlo);
  
  -- Single-driven assignments
  aoltjzsvk <= 16#8281.7_5_6# ps;
  mjgz <= (2#0_1_0.1_0_1_0# ps, 8#4# fs);
  kwuedwpiu <= kwuedwpiu;
  
  -- Multi-driven assignments
  sx <= sx;
  sx <= sx;
  sx <= sx;
  sx <= sx;
end ibj;

library ieee;
use ieee.std_logic_1164.all;

entity yweabbfc is
  port (sglayau : linkage std_logic; ybjzfhki : inout boolean_vector(2 to 1); yywvfmto : buffer time; mev : linkage std_logic_vector(0 to 4));
end yweabbfc;

library ieee;
use ieee.std_logic_1164.all;

architecture wamepl of yweabbfc is
  signal fboq : time_vector(1 downto 1);
  signal fjv : real;
  signal i : time_vector(1 downto 1);
  signal zp : real;
  signal ka : time_vector(1 to 2);
  signal fdupceqga : time_vector(1 downto 1);
  signal vwlgfdbr : real;
  signal jltqgj : time_vector(1 downto 1);
  signal nd : real;
  signal dgw : time_vector(1 to 2);
  signal hafrmzthgc : std_logic_vector(4 downto 2);
begin
  isew : entity work.eekvuemr
    port map (suaen => hafrmzthgc, jutdapscib => dgw, bty => nd, t => jltqgj);
  xqxmyfoq : entity work.eekvuemr
    port map (suaen => hafrmzthgc, jutdapscib => dgw, bty => vwlgfdbr, t => fdupceqga);
  g : entity work.eekvuemr
    port map (suaen => hafrmzthgc, jutdapscib => ka, bty => zp, t => i);
  cejeg : entity work.eekvuemr
    port map (suaen => hafrmzthgc, jutdapscib => dgw, bty => fjv, t => fboq);
  
  -- Multi-driven assignments
  hafrmzthgc <= hafrmzthgc;
  hafrmzthgc <= ('W', '0', '-');
end wamepl;

entity u is
  port (zpt : inout real);
end u;

library ieee;
use ieee.std_logic_1164.all;

architecture qrjlknesjq of u is
  signal lov : time_vector(1 downto 1);
  signal ikwxgxnk : time_vector(1 to 2);
  signal zow : std_logic_vector(4 downto 2);
  signal ulmgy : real;
  signal mr : time;
  signal g : std_logic_vector(2 to 4);
  signal gfe : real;
  signal llxsu : time;
  signal nkratf : std_logic_vector(2 to 4);
begin
  bkixbsmcni : entity work.awmjqgb
    port map (sx => nkratf, aoltjzsvk => llxsu, mahmticye => gfe);
  pop : entity work.awmjqgb
    port map (sx => g, aoltjzsvk => mr, mahmticye => ulmgy);
  xlglj : entity work.eekvuemr
    port map (suaen => zow, jutdapscib => ikwxgxnk, bty => zpt, t => lov);
  
  -- Single-driven assignments
  ikwxgxnk <= (2#000# us, 1 ms);
  
  -- Multi-driven assignments
  g <= "XXU";
  nkratf <= nkratf;
end qrjlknesjq;



-- Seed after: 7145144802235086609,5906004015519833893
