-- Seed: 5861886347611373324,10975587770517081373

library ieee;
use ieee.std_logic_1164.all;

entity ug is
  port (hfeihzthgy : in time_vector(4 downto 1); gac : inout time; ogequu : linkage integer; s : linkage std_logic);
end ug;

architecture vataatzwtl of ug is
  
begin
  -- Single-driven assignments
  gac <= 0_3_2_2 ms;
end vataatzwtl;

library ieee;
use ieee.std_logic_1164.all;

entity vtndxqbx is
  port (gyonkzum : in std_logic);
end vtndxqbx;

library ieee;
use ieee.std_logic_1164.all;

architecture dhxph of vtndxqbx is
  signal lohhxl : std_logic;
  signal icskbxd : integer;
  signal svxyocyb : time;
  signal a : std_logic;
  signal kki : integer;
  signal ksrg : time;
  signal dj : time_vector(4 downto 1);
  signal itplqjaai : integer;
  signal d : time;
  signal kfzgvtf : time_vector(4 downto 1);
begin
  iz : entity work.ug
    port map (hfeihzthgy => kfzgvtf, gac => d, ogequu => itplqjaai, s => gyonkzum);
  xrzvz : entity work.ug
    port map (hfeihzthgy => dj, gac => ksrg, ogequu => kki, s => a);
  s : entity work.ug
    port map (hfeihzthgy => dj, gac => svxyocyb, ogequu => icskbxd, s => lohhxl);
  
  -- Single-driven assignments
  kfzgvtf <= (3_2_3.11200 us, 3441 us, 2 sec, 23202.0_4_4_4_1 us);
  
  -- Multi-driven assignments
  lohhxl <= gyonkzum;
  a <= '0';
  lohhxl <= gyonkzum;
end dhxph;



-- Seed after: 2672247279966259265,10975587770517081373
