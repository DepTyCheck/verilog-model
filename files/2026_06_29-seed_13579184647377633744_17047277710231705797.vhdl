-- Seed: 13579184647377633744,17047277710231705797

entity ukbgg is
  port (sd : out integer_vector(4 to 2); xdc : inout real);
end ukbgg;

architecture bixkqk of ukbgg is
  
begin
  
end bixkqk;

library ieee;
use ieee.std_logic_1164.all;

entity ejesznokd is
  port (yi : buffer std_logic; xeeaeys : out std_logic_vector(0 to 3); pctdcmpv : inout bit);
end ejesznokd;

architecture shqqaa of ejesznokd is
  signal i : real;
  signal dpu : integer_vector(4 to 2);
  signal dtb : real;
  signal uahzup : integer_vector(4 to 2);
begin
  uoxaz : entity work.ukbgg
    port map (sd => uahzup, xdc => dtb);
  upq : entity work.ukbgg
    port map (sd => dpu, xdc => i);
  
  -- Single-driven assignments
  pctdcmpv <= '0';
  
  -- Multi-driven assignments
  xeeaeys <= "XX-W";
  xeeaeys <= ('H', 'H', '-', '1');
  xeeaeys <= "WLH0";
  xeeaeys <= "L-X0";
end shqqaa;



-- Seed after: 4113087036893729390,17047277710231705797
