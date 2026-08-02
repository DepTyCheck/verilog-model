-- Seed: 2670935803128455259,13592003931158285879

entity touqwenx is
  port (ociadqepmd : buffer character; syvdjca : linkage real_vector(3 downto 2));
end touqwenx;

architecture trfx of touqwenx is
  
begin
  -- Single-driven assignments
  ociadqepmd <= ociadqepmd;
end trfx;

library ieee;
use ieee.std_logic_1164.all;

entity d is
  port (bl : linkage real; zqurgz : inout integer_vector(0 to 3); bytddwmdac : out std_logic; qufvsd : inout integer);
end d;

architecture pjhsnqbwh of d is
  signal uui : real_vector(3 downto 2);
  signal xr : character;
  signal kwvcajpnbd : real_vector(3 downto 2);
  signal fppmmyq : character;
  signal axg : real_vector(3 downto 2);
  signal w : character;
  signal nkadnolbqe : real_vector(3 downto 2);
  signal bpsqcv : character;
begin
  vs : entity work.touqwenx
    port map (ociadqepmd => bpsqcv, syvdjca => nkadnolbqe);
  kaim : entity work.touqwenx
    port map (ociadqepmd => w, syvdjca => axg);
  aitv : entity work.touqwenx
    port map (ociadqepmd => fppmmyq, syvdjca => kwvcajpnbd);
  zjryzcdad : entity work.touqwenx
    port map (ociadqepmd => xr, syvdjca => uui);
  
  -- Single-driven assignments
  qufvsd <= qufvsd;
  
  -- Multi-driven assignments
  bytddwmdac <= 'L';
  bytddwmdac <= 'U';
  bytddwmdac <= bytddwmdac;
end pjhsnqbwh;

library ieee;
use ieee.std_logic_1164.all;

entity ul is
  port (tzf : in std_logic);
end ul;

architecture c of ul is
  
begin
  
end c;

library ieee;
use ieee.std_logic_1164.all;

entity lidprllgel is
  port (yon : in bit; lyvscvt : buffer std_logic; mbdc : in std_logic);
end lidprllgel;

architecture qippcxc of lidprllgel is
  signal epaftrfb : real_vector(3 downto 2);
  signal w : character;
  signal arg : real_vector(3 downto 2);
  signal aolzicds : character;
  signal qnyrbr : integer;
  signal ajworq : integer_vector(0 to 3);
  signal rbnowczsgv : real;
begin
  zw : entity work.d
    port map (bl => rbnowczsgv, zqurgz => ajworq, bytddwmdac => lyvscvt, qufvsd => qnyrbr);
  bwhpssj : entity work.touqwenx
    port map (ociadqepmd => aolzicds, syvdjca => arg);
  yv : entity work.ul
    port map (tzf => mbdc);
  oambl : entity work.touqwenx
    port map (ociadqepmd => w, syvdjca => epaftrfb);
  
  -- Multi-driven assignments
  lyvscvt <= 'L';
  lyvscvt <= '-';
  lyvscvt <= '0';
  lyvscvt <= mbdc;
end qippcxc;



-- Seed after: 13389859747676953019,13592003931158285879
