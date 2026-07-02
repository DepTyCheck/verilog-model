-- Seed: 16200984426685884504,13694093582652240945

library ieee;
use ieee.std_logic_1164.all;

entity ui is
  port (wtrftbouze : inout std_logic; ftmzjobbif : inout string(4 downto 3));
end ui;

architecture pmvunsclj of ui is
  
begin
  -- Single-driven assignments
  ftmzjobbif <= "pe";
  
  -- Multi-driven assignments
  wtrftbouze <= 'W';
  wtrftbouze <= '1';
end pmvunsclj;

library ieee;
use ieee.std_logic_1164.all;

entity dyndfgdbm is
  port (n : inout std_logic_vector(1 downto 3); splngvjou : inout std_logic);
end dyndfgdbm;

library ieee;
use ieee.std_logic_1164.all;

architecture nsjbwwvm of dyndfgdbm is
  signal rfohmpab : string(4 downto 3);
  signal fpmvgasg : std_logic;
  signal ywlsqyij : string(4 downto 3);
  signal vg : std_logic;
  signal ckwjujqca : string(4 downto 3);
  signal mwrrrq : std_logic;
  signal xfmhl : string(4 downto 3);
  signal rjiugmryc : std_logic;
begin
  ldldxw : entity work.ui
    port map (wtrftbouze => rjiugmryc, ftmzjobbif => xfmhl);
  wzfrcvdidu : entity work.ui
    port map (wtrftbouze => mwrrrq, ftmzjobbif => ckwjujqca);
  xbmnljkyg : entity work.ui
    port map (wtrftbouze => vg, ftmzjobbif => ywlsqyij);
  sx : entity work.ui
    port map (wtrftbouze => fpmvgasg, ftmzjobbif => rfohmpab);
  
  -- Multi-driven assignments
  splngvjou <= 'L';
  splngvjou <= 'H';
end nsjbwwvm;

entity lhssrm is
  port (ojyit : out bit; n : in real; ecxhfj : in integer);
end lhssrm;

library ieee;
use ieee.std_logic_1164.all;

architecture bdksw of lhssrm is
  signal jokezakc : string(4 downto 3);
  signal pybubwzl : std_logic;
  signal tyxdzxgxo : std_logic;
  signal hiupea : std_logic_vector(1 downto 3);
  signal ta : std_logic_vector(1 downto 3);
  signal b : string(4 downto 3);
  signal fvtwew : std_logic;
begin
  rtmc : entity work.ui
    port map (wtrftbouze => fvtwew, ftmzjobbif => b);
  xgqfbrnow : entity work.dyndfgdbm
    port map (n => ta, splngvjou => fvtwew);
  a : entity work.dyndfgdbm
    port map (n => hiupea, splngvjou => tyxdzxgxo);
  mihdoxoeic : entity work.ui
    port map (wtrftbouze => pybubwzl, ftmzjobbif => jokezakc);
  
  -- Single-driven assignments
  ojyit <= '1';
end bdksw;



-- Seed after: 12451123479324966371,13694093582652240945
