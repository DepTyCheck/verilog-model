-- Seed: 11877187853032967288,12011142928354116943

entity uplefrpc is
  port (z : out bit; mhufrx : out real; saz : in integer_vector(2 downto 4));
end uplefrpc;

architecture jlhxak of uplefrpc is
  
begin
  -- Single-driven assignments
  mhufrx <= 8#2_0.0#;
  z <= '1';
end jlhxak;

entity jdqcz is
  port (oxp : buffer real; r : out real_vector(3 to 4));
end jdqcz;

architecture b of jdqcz is
  signal ggujcvqwij : integer_vector(2 downto 4);
  signal brvio : bit;
begin
  dtdumnelf : entity work.uplefrpc
    port map (z => brvio, mhufrx => oxp, saz => ggujcvqwij);
  
  -- Single-driven assignments
  ggujcvqwij <= (others => 0);
  r <= (2#0_0_0_0_1.0_0_1#, 3_2_4.2);
end b;

library ieee;
use ieee.std_logic_1164.all;

entity eqvxi is
  port (ttnnsfpbe : buffer integer; orkrnu : inout std_logic; iwtw : out real; ayhoyjl : out real_vector(4 downto 3));
end eqvxi;

architecture nrjqdoaj of eqvxi is
  signal jya : integer_vector(2 downto 4);
  signal xqzme : real;
  signal zg : bit;
  signal oib : integer_vector(2 downto 4);
  signal mcldyzaep : real;
  signal oa : bit;
begin
  rvgpvypuc : entity work.uplefrpc
    port map (z => oa, mhufrx => mcldyzaep, saz => oib);
  zpfozpkhp : entity work.uplefrpc
    port map (z => zg, mhufrx => xqzme, saz => jya);
end nrjqdoaj;



-- Seed after: 394602118806693785,12011142928354116943
