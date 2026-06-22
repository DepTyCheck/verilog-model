-- Seed: 3755586829061954548,13479070923501788437

entity wnjmsvxiuq is
  port (i : in boolean_vector(4 downto 4); qktkgldmng : buffer time_vector(1 to 1); rr : inout real);
end wnjmsvxiuq;

architecture ovsi of wnjmsvxiuq is
  
begin
  -- Single-driven assignments
  rr <= 0103.0_2_2_4;
  qktkgldmng <= (others => 16#F_B_2# ms);
end ovsi;

library ieee;
use ieee.std_logic_1164.all;

entity h is
  port (hvolelo : out real; redpdttaz : out time; tvtmijv : out std_logic);
end h;

architecture vufl of h is
  
begin
  -- Single-driven assignments
  redpdttaz <= 2130.0112 ps;
  hvolelo <= 0.2_0_3_4_2;
  
  -- Multi-driven assignments
  tvtmijv <= 'H';
end vufl;

library ieee;
use ieee.std_logic_1164.all;

entity iprwr is
  port (q : out std_logic; pptvkceao : in std_logic_vector(2 to 3));
end iprwr;

architecture qbqkcg of iprwr is
  signal ra : real;
  signal j : time_vector(1 to 1);
  signal igkb : boolean_vector(4 downto 4);
begin
  kuks : entity work.wnjmsvxiuq
    port map (i => igkb, qktkgldmng => j, rr => ra);
  
  -- Single-driven assignments
  igkb <= (others => FALSE);
  
  -- Multi-driven assignments
  q <= '1';
  q <= 'Z';
  q <= 'U';
  q <= '1';
end qbqkcg;



-- Seed after: 16375463766933815574,13479070923501788437
