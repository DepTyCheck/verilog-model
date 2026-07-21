-- Seed: 14971931401081270648,11481034001933599325

library ieee;
use ieee.std_logic_1164.all;

entity rinr is
  port (w : buffer integer; jxpkddtsmv : in integer; c : buffer std_logic_vector(3 downto 4));
end rinr;

architecture jsanr of rinr is
  
begin
  -- Single-driven assignments
  w <= 16#B#;
  
  -- Multi-driven assignments
  c <= (others => '0');
end jsanr;

entity xd is
  port (dqvcdwv : out real);
end xd;

library ieee;
use ieee.std_logic_1164.all;

architecture jrlhdn of xd is
  signal nqwrdu : integer;
  signal dea : std_logic_vector(3 downto 4);
  signal ong : integer;
  signal xuhlvk : integer;
begin
  aazkexh : entity work.rinr
    port map (w => xuhlvk, jxpkddtsmv => ong, c => dea);
  lo : entity work.rinr
    port map (w => ong, jxpkddtsmv => nqwrdu, c => dea);
  uwupbyncqu : entity work.rinr
    port map (w => nqwrdu, jxpkddtsmv => xuhlvk, c => dea);
  
  -- Single-driven assignments
  dqvcdwv <= 8#5.1_6_4_0#;
  
  -- Multi-driven assignments
  dea <= (others => '0');
end jrlhdn;



-- Seed after: 1758641324120693114,11481034001933599325
