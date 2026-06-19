-- Seed: 14833861640961952906,3108530264173481209

entity h is
  port (yjdi : in real; p : inout time);
end h;

architecture mz of h is
  
begin
  -- Single-driven assignments
  p <= 16#F6F.B_E_D_0# fs;
end mz;

library ieee;
use ieee.std_logic_1164.all;

entity vqyqolpaa is
  port (cloloao : inout real_vector(2 to 0); rok : buffer boolean; ybo : in string(2 downto 5); vs : out std_logic_vector(2 downto 3));
end vqyqolpaa;

architecture osjkhaiw of vqyqolpaa is
  signal gqbqgl : time;
  signal yvlbnsvp : time;
  signal nracxzqo : real;
begin
  tziowmtxzp : entity work.h
    port map (yjdi => nracxzqo, p => yvlbnsvp);
  vyhft : entity work.h
    port map (yjdi => nracxzqo, p => gqbqgl);
  
  -- Single-driven assignments
  cloloao <= (others => 0.0);
  rok <= FALSE;
  
  -- Multi-driven assignments
  vs <= (others => '0');
  vs <= "";
  vs <= (others => '0');
end osjkhaiw;



-- Seed after: 16588627971253346553,3108530264173481209
