-- Seed: 8425945765298187220,5906004015519833893

entity dlih is
  port (fdkorb : inout real_vector(4 downto 2));
end dlih;

architecture vaipgdozpb of dlih is
  
begin
  -- Single-driven assignments
  fdkorb <= fdkorb;
end vaipgdozpb;

library ieee;
use ieee.std_logic_1164.all;

entity wrmy is
  port (vbeuls : inout std_logic_vector(2 downto 0));
end wrmy;

architecture vo of wrmy is
  
begin
  -- Multi-driven assignments
  vbeuls <= vbeuls;
  vbeuls <= ('H', '1', 'Z');
  vbeuls <= ('-', 'H', '-');
  vbeuls <= vbeuls;
end vo;

library ieee;
use ieee.std_logic_1164.all;

entity zc is
  port (tuebxqum : buffer time_vector(0 downto 4); sctvdhf : linkage std_logic; mxxqvrxjd : out std_logic_vector(0 to 0));
end zc;

architecture eszzei of zc is
  signal qaqpcfxts : real_vector(4 downto 2);
  signal uiphxl : real_vector(4 downto 2);
begin
  vhx : entity work.dlih
    port map (fdkorb => uiphxl);
  ksprej : entity work.dlih
    port map (fdkorb => qaqpcfxts);
  
  -- Single-driven assignments
  tuebxqum <= (others => 0 ns);
  
  -- Multi-driven assignments
  mxxqvrxjd <= (others => 'Z');
  mxxqvrxjd <= "0";
  mxxqvrxjd <= "W";
  mxxqvrxjd <= (others => '0');
end eszzei;

entity gd is
  port (p : inout bit);
end gd;

library ieee;
use ieee.std_logic_1164.all;

architecture xexpihwv of gd is
  signal hwcxcsu : std_logic_vector(2 downto 0);
begin
  nruul : entity work.wrmy
    port map (vbeuls => hwcxcsu);
  
  -- Multi-driven assignments
  hwcxcsu <= hwcxcsu;
  hwcxcsu <= ('Z', '1', 'X');
  hwcxcsu <= hwcxcsu;
end xexpihwv;



-- Seed after: 631572462133869973,5906004015519833893
