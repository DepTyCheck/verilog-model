-- Seed: 12753699328901245802,13592003931158285879

library ieee;
use ieee.std_logic_1164.all;

entity gkhuv is
  port (hpehbzp : in std_logic_vector(4 downto 0); e : in time);
end gkhuv;

architecture k of gkhuv is
  
begin
  
end k;

library ieee;
use ieee.std_logic_1164.all;

entity p is
  port (jluw : linkage bit; z : in std_logic; gzoqntvvx : inout time; xzkccfk : out real);
end p;

library ieee;
use ieee.std_logic_1164.all;

architecture otius of p is
  signal wmgdoeuaoa : std_logic_vector(4 downto 0);
  signal srm : time;
  signal ngqjvsqib : std_logic_vector(4 downto 0);
  signal zw : std_logic_vector(4 downto 0);
begin
  ot : entity work.gkhuv
    port map (hpehbzp => zw, e => gzoqntvvx);
  uljyw : entity work.gkhuv
    port map (hpehbzp => ngqjvsqib, e => srm);
  udlu : entity work.gkhuv
    port map (hpehbzp => wmgdoeuaoa, e => gzoqntvvx);
  
  -- Single-driven assignments
  gzoqntvvx <= srm;
  
  -- Multi-driven assignments
  zw <= ('-', 'H', 'H', '-', '0');
  wmgdoeuaoa <= ngqjvsqib;
  zw <= ('0', 'U', 'Z', 'U', 'Z');
end otius;

library ieee;
use ieee.std_logic_1164.all;

entity spnzz is
  port (hrzfr : inout std_logic_vector(0 downto 0); kc : inout bit; kzjet : buffer std_logic; bzyppve : buffer severity_level);
end spnzz;

library ieee;
use ieee.std_logic_1164.all;

architecture mvttadbk of spnzz is
  signal jckejqvgxj : time;
  signal dxhvts : std_logic_vector(4 downto 0);
begin
  xsldcqmf : entity work.gkhuv
    port map (hpehbzp => dxhvts, e => jckejqvgxj);
  
  -- Multi-driven assignments
  dxhvts <= dxhvts;
  kzjet <= kzjet;
  kzjet <= kzjet;
  dxhvts <= ('Z', 'Z', 'Z', '-', 'L');
end mvttadbk;



-- Seed after: 6078707627504485164,13592003931158285879
