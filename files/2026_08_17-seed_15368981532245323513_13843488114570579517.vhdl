-- Seed: 15368981532245323513,13843488114570579517

library ieee;
use ieee.std_logic_1164.all;

entity lhnc is
  port (pzr : linkage time; ektab : inout boolean; btir : out std_logic_vector(1 downto 4); aaahp : inout real_vector(1 to 0));
end lhnc;

architecture jbdnms of lhnc is
  
begin
  -- Single-driven assignments
  aaahp <= (others => 0.0);
  ektab <= ektab;
end jbdnms;

library ieee;
use ieee.std_logic_1164.all;

entity zazyfbu is
  port (armvhzw : linkage std_logic; rwdgiit : in time; dx : in std_logic);
end zazyfbu;

library ieee;
use ieee.std_logic_1164.all;

architecture tlcnw of zazyfbu is
  signal hxkdhswun : real_vector(1 to 0);
  signal lfzue : boolean;
  signal p : time;
  signal tk : real_vector(1 to 0);
  signal vlicvh : std_logic_vector(1 downto 4);
  signal wlisluzzdv : boolean;
  signal ij : time;
begin
  dvecifenbq : entity work.lhnc
    port map (pzr => ij, ektab => wlisluzzdv, btir => vlicvh, aaahp => tk);
  hnxphirz : entity work.lhnc
    port map (pzr => p, ektab => lfzue, btir => vlicvh, aaahp => hxkdhswun);
  
  -- Multi-driven assignments
  vlicvh <= (others => '0');
  vlicvh <= "";
end tlcnw;

library ieee;
use ieee.std_logic_1164.all;

entity brvqjrf is
  port (eciccymn : in real; xxmgwsevfb : in std_logic);
end brvqjrf;

library ieee;
use ieee.std_logic_1164.all;

architecture lcji of brvqjrf is
  signal dog : real_vector(1 to 0);
  signal lbwcpzuarc : std_logic_vector(1 downto 4);
  signal mybkgmfta : boolean;
  signal go : time;
begin
  juwhkjc : entity work.zazyfbu
    port map (armvhzw => xxmgwsevfb, rwdgiit => go, dx => xxmgwsevfb);
  escnfuodu : entity work.lhnc
    port map (pzr => go, ektab => mybkgmfta, btir => lbwcpzuarc, aaahp => dog);
  
  -- Multi-driven assignments
  lbwcpzuarc <= (others => '0');
  lbwcpzuarc <= lbwcpzuarc;
  lbwcpzuarc <= "";
end lcji;

library ieee;
use ieee.std_logic_1164.all;

entity hzmigfo is
  port (rbhgj : linkage bit; m : out std_logic_vector(0 downto 2); uwpfavgues : linkage boolean);
end hzmigfo;

library ieee;
use ieee.std_logic_1164.all;

architecture renistfo of hzmigfo is
  signal cxorshm : std_logic;
  signal nvqe : real;
  signal gty : std_logic;
  signal xmq : real;
begin
  msaeeorrn : entity work.brvqjrf
    port map (eciccymn => xmq, xxmgwsevfb => gty);
  axmdm : entity work.brvqjrf
    port map (eciccymn => nvqe, xxmgwsevfb => cxorshm);
  
  -- Single-driven assignments
  xmq <= xmq;
  nvqe <= 16#1_C_6.D_5_B_C_F#;
  
  -- Multi-driven assignments
  m <= m;
  gty <= gty;
  m <= "";
  m <= m;
end renistfo;



-- Seed after: 12900306817404827349,13843488114570579517
