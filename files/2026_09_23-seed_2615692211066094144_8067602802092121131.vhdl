-- Seed: 2615692211066094144,8067602802092121131

library ieee;
use ieee.std_logic_1164.all;

entity tulule is
  port (chik : inout time_vector(2 to 0); lgjfjzby : out std_logic_vector(2 downto 0));
end tulule;

architecture s of tulule is
  
begin
  -- Single-driven assignments
  chik <= (others => 0 ns);
  
  -- Multi-driven assignments
  lgjfjzby <= "Z00";
  lgjfjzby <= lgjfjzby;
end s;

entity hnhqhc is
  port (ohbmzaub : buffer real);
end hnhqhc;

library ieee;
use ieee.std_logic_1164.all;

architecture h of hnhqhc is
  signal rtebp : std_logic_vector(2 downto 0);
  signal xq : time_vector(2 to 0);
  signal kvbeb : std_logic_vector(2 downto 0);
  signal pnownsdbti : time_vector(2 to 0);
begin
  zlzdj : entity work.tulule
    port map (chik => pnownsdbti, lgjfjzby => kvbeb);
  yqgpkpqtlh : entity work.tulule
    port map (chik => xq, lgjfjzby => rtebp);
  
  -- Single-driven assignments
  ohbmzaub <= 8#2_2.1_1_5#;
end h;



-- Seed after: 4024892700384538408,8067602802092121131
