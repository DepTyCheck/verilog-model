-- Seed: 16564201784196038954,6697892553037813751

library ieee;
use ieee.std_logic_1164.all;

entity udcowyrm is
  port (zfulkkda : in std_logic_vector(2 downto 1); nej : linkage std_logic_vector(1 downto 0); tr : inout std_logic; rylciggkj : inout std_logic);
end udcowyrm;

architecture ugdc of udcowyrm is
  
begin
  -- Multi-driven assignments
  rylciggkj <= 'X';
  rylciggkj <= 'X';
  rylciggkj <= '-';
  rylciggkj <= '0';
end ugdc;

library ieee;
use ieee.std_logic_1164.all;

entity vyna is
  port (t : out std_logic; gvezeitf : out real);
end vyna;

library ieee;
use ieee.std_logic_1164.all;

architecture jl of vyna is
  signal y : std_logic;
  signal yxw : std_logic_vector(2 downto 1);
  signal euyyefzy : std_logic;
  signal pns : std_logic_vector(1 downto 0);
  signal lenk : std_logic;
  signal cy : std_logic;
  signal xdsktn : std_logic;
  signal ucikaj : std_logic;
  signal ehacecmwgb : std_logic_vector(2 downto 1);
  signal iulfsebhaa : std_logic_vector(2 downto 1);
begin
  yshvmgak : entity work.udcowyrm
    port map (zfulkkda => iulfsebhaa, nej => ehacecmwgb, tr => ucikaj, rylciggkj => xdsktn);
  ghnb : entity work.udcowyrm
    port map (zfulkkda => ehacecmwgb, nej => iulfsebhaa, tr => cy, rylciggkj => lenk);
  wrd : entity work.udcowyrm
    port map (zfulkkda => iulfsebhaa, nej => pns, tr => xdsktn, rylciggkj => euyyefzy);
  h : entity work.udcowyrm
    port map (zfulkkda => yxw, nej => pns, tr => y, rylciggkj => t);
  
  -- Single-driven assignments
  gvezeitf <= 33232.4_0_1_0;
end jl;



-- Seed after: 41836563681245547,6697892553037813751
