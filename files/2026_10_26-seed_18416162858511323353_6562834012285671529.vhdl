-- Seed: 18416162858511323353,6562834012285671529

library ieee;
use ieee.std_logic_1164.all;

entity umu is
  port (g : out std_logic; t : buffer std_logic_vector(2 downto 4); aibhkxgc : in std_logic_vector(0 downto 4));
end umu;

architecture bfok of umu is
  
begin
  -- Multi-driven assignments
  t <= t;
  t <= aibhkxgc;
  g <= '1';
end bfok;

library ieee;
use ieee.std_logic_1164.all;

entity gn is
  port (scjiwhpla : buffer real; dxxirebkfj : inout std_logic; ctdd : in std_logic_vector(0 downto 3); t : in time);
end gn;

library ieee;
use ieee.std_logic_1164.all;

architecture m of gn is
  signal nvscbfrq : std_logic_vector(0 downto 4);
  signal g : std_logic_vector(2 downto 4);
  signal aomijkrv : std_logic_vector(0 downto 4);
  signal lskforin : std_logic_vector(2 downto 4);
begin
  zp : entity work.umu
    port map (g => dxxirebkfj, t => lskforin, aibhkxgc => aomijkrv);
  jt : entity work.umu
    port map (g => dxxirebkfj, t => g, aibhkxgc => nvscbfrq);
  qa : entity work.umu
    port map (g => dxxirebkfj, t => lskforin, aibhkxgc => nvscbfrq);
  
  -- Single-driven assignments
  scjiwhpla <= 16#2.84#;
  
  -- Multi-driven assignments
  aomijkrv <= "";
end m;

entity dkvfyzdjy is
  port (pj : out bit);
end dkvfyzdjy;

library ieee;
use ieee.std_logic_1164.all;

architecture pjzapki of dkvfyzdjy is
  signal jf : std_logic_vector(0 downto 4);
  signal ntske : time;
  signal tgjk : std_logic_vector(2 downto 4);
  signal gscpsd : std_logic;
  signal ycxulzrza : real;
begin
  jlxtposf : entity work.gn
    port map (scjiwhpla => ycxulzrza, dxxirebkfj => gscpsd, ctdd => tgjk, t => ntske);
  zj : entity work.umu
    port map (g => gscpsd, t => tgjk, aibhkxgc => jf);
  
  -- Multi-driven assignments
  gscpsd <= gscpsd;
  gscpsd <= 'H';
end pjzapki;

entity zzqclto is
  port (v : inout boolean_vector(4 downto 4); qlfhvr : linkage character);
end zzqclto;

library ieee;
use ieee.std_logic_1164.all;

architecture j of zzqclto is
  signal jj : std_logic_vector(0 downto 4);
  signal x : std_logic;
begin
  ovpqum : entity work.umu
    port map (g => x, t => jj, aibhkxgc => jj);
  
  -- Multi-driven assignments
  jj <= jj;
  jj <= jj;
end j;



-- Seed after: 12824349889105286821,6562834012285671529
