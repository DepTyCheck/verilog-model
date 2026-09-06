-- Seed: 14121078943146934280,14094562573555574003

library ieee;
use ieee.std_logic_1164.all;

entity wwfml is
  port (rmtkait : linkage time; rzcjiw : out time; oqtpdhgk : in std_logic_vector(3 downto 4));
end wwfml;

architecture haxouo of wwfml is
  
begin
  -- Single-driven assignments
  rzcjiw <= 0 min;
end haxouo;

library ieee;
use ieee.std_logic_1164.all;

entity fiygrlnu is
  port (bwr : out std_logic_vector(1 to 0); v : out severity_level);
end fiygrlnu;

architecture xwagtq of fiygrlnu is
  
begin
  -- Single-driven assignments
  v <= WARNING;
  
  -- Multi-driven assignments
  bwr <= bwr;
end xwagtq;

entity fftqqvlwh is
  port (zsofpsn : linkage boolean; zdmdep : in integer_vector(1 downto 2));
end fftqqvlwh;

library ieee;
use ieee.std_logic_1164.all;

architecture vioezcn of fftqqvlwh is
  signal racdo : std_logic_vector(3 downto 4);
  signal ubyra : time;
  signal q : time;
  signal djkn : severity_level;
  signal cian : std_logic_vector(1 to 0);
  signal lp : severity_level;
  signal lrrhzac : std_logic_vector(1 to 0);
begin
  mvrmw : entity work.fiygrlnu
    port map (bwr => lrrhzac, v => lp);
  ed : entity work.fiygrlnu
    port map (bwr => cian, v => djkn);
  oxu : entity work.wwfml
    port map (rmtkait => q, rzcjiw => ubyra, oqtpdhgk => racdo);
  
  -- Multi-driven assignments
  lrrhzac <= lrrhzac;
  cian <= lrrhzac;
  lrrhzac <= lrrhzac;
  cian <= "";
end vioezcn;

library ieee;
use ieee.std_logic_1164.all;

entity mzfeap is
  port (fqwnhyhne : inout std_logic; pmnieb : inout time; bnlm : linkage std_logic_vector(0 downto 1));
end mzfeap;

architecture thbr of mzfeap is
  signal c : integer_vector(1 downto 2);
  signal htjtr : boolean;
begin
  zcr : entity work.fftqqvlwh
    port map (zsofpsn => htjtr, zdmdep => c);
  
  -- Multi-driven assignments
  fqwnhyhne <= 'Z';
  fqwnhyhne <= fqwnhyhne;
  fqwnhyhne <= 'H';
  fqwnhyhne <= fqwnhyhne;
end thbr;



-- Seed after: 1885043814044597744,14094562573555574003
