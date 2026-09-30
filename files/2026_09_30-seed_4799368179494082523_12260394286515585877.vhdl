-- Seed: 4799368179494082523,12260394286515585877

library ieee;
use ieee.std_logic_1164.all;

entity miweloxp is
  port (eh : out real; ym : buffer string(3 downto 2); zq : out std_logic; c : linkage integer);
end miweloxp;

architecture exkrg of miweloxp is
  
begin
  
end exkrg;

entity gdrggbd is
  port (zzxrl : buffer integer);
end gdrggbd;

library ieee;
use ieee.std_logic_1164.all;

architecture qrtio of gdrggbd is
  signal tksi : integer;
  signal u : std_logic;
  signal eozjjs : string(3 downto 2);
  signal ozwvkjxy : real;
begin
  bbbbod : entity work.miweloxp
    port map (eh => ozwvkjxy, ym => eozjjs, zq => u, c => tksi);
  
  -- Multi-driven assignments
  u <= 'H';
  u <= u;
  u <= 'Z';
end qrtio;

entity grzi is
  port (wumxcw : buffer integer);
end grzi;

library ieee;
use ieee.std_logic_1164.all;

architecture m of grzi is
  signal ekelehl : integer;
  signal vgpzmot : std_logic;
  signal jexyzhu : string(3 downto 2);
  signal fwcsm : real;
begin
  camufiim : entity work.miweloxp
    port map (eh => fwcsm, ym => jexyzhu, zq => vgpzmot, c => ekelehl);
  v : entity work.gdrggbd
    port map (zzxrl => wumxcw);
  
  -- Multi-driven assignments
  vgpzmot <= '1';
  vgpzmot <= 'W';
  vgpzmot <= 'U';
end m;

entity kau is
  port (qzlt : buffer bit_vector(3 downto 4));
end kau;

library ieee;
use ieee.std_logic_1164.all;

architecture hlqety of kau is
  signal mhzfrbsvkj : integer;
  signal hrla : std_logic;
  signal p : string(3 downto 2);
  signal rbggmjn : real;
  signal aitdpiny : integer;
  signal uf : integer;
  signal ri : integer;
begin
  vahrd : entity work.gdrggbd
    port map (zzxrl => ri);
  tb : entity work.grzi
    port map (wumxcw => uf);
  zezkhy : entity work.gdrggbd
    port map (zzxrl => aitdpiny);
  k : entity work.miweloxp
    port map (eh => rbggmjn, ym => p, zq => hrla, c => mhzfrbsvkj);
  
  -- Single-driven assignments
  qzlt <= (others => '0');
  
  -- Multi-driven assignments
  hrla <= hrla;
  hrla <= 'W';
  hrla <= hrla;
end hlqety;



-- Seed after: 5045548522989464444,12260394286515585877
