-- Seed: 5694802592858736651,8067602802092121131

library ieee;
use ieee.std_logic_1164.all;

entity rgqlva is
  port (hqhhkc : linkage std_logic_vector(1 downto 3));
end rgqlva;

architecture ffculxxb of rgqlva is
  
begin
  
end ffculxxb;

entity avbbdpv is
  port (bomdpi : linkage character; jb : linkage integer);
end avbbdpv;

library ieee;
use ieee.std_logic_1164.all;

architecture o of avbbdpv is
  signal zwtk : std_logic_vector(1 downto 3);
  signal ovn : std_logic_vector(1 downto 3);
begin
  ktnajbrsx : entity work.rgqlva
    port map (hqhhkc => ovn);
  quuzcjfxsz : entity work.rgqlva
    port map (hqhhkc => zwtk);
end o;

entity jzasc is
  port (mvk : in boolean_vector(1 to 3); izn : inout integer_vector(2 downto 4); e : in real_vector(1 downto 1); y : in boolean);
end jzasc;

library ieee;
use ieee.std_logic_1164.all;

architecture zz of jzasc is
  signal pg : std_logic_vector(1 downto 3);
  signal xftij : std_logic_vector(1 downto 3);
begin
  qihs : entity work.rgqlva
    port map (hqhhkc => xftij);
  srb : entity work.rgqlva
    port map (hqhhkc => pg);
  
  -- Single-driven assignments
  izn <= (others => 0);
  
  -- Multi-driven assignments
  xftij <= (others => '0');
  pg <= pg;
  xftij <= "";
end zz;



-- Seed after: 16735811427487177951,8067602802092121131
