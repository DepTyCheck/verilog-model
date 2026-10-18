-- Seed: 5767184991900280173,14108379317686958507

entity oijxnajm is
  port (f : out string(4 downto 3); uqplel : buffer integer; niao : out time_vector(0 to 3));
end oijxnajm;

architecture tdijbpcn of oijxnajm is
  
begin
  -- Single-driven assignments
  niao <= (8#5_5_5_0.4304# us, 1_3_4 us, 8#5.5_1_3_6# us, 4 min);
  uqplel <= uqplel;
end tdijbpcn;

library ieee;
use ieee.std_logic_1164.all;

entity mqwhnzqkyo is
  port (oq : inout std_logic_vector(3 to 3); ysuuapb : out integer);
end mqwhnzqkyo;

architecture ihk of mqwhnzqkyo is
  signal wxpa : time_vector(0 to 3);
  signal avkmbt : string(4 downto 3);
  signal bjexcejsaf : time_vector(0 to 3);
  signal thcolk : integer;
  signal yl : string(4 downto 3);
  signal pbwwkb : time_vector(0 to 3);
  signal eumvjxtqho : integer;
  signal ishphaef : string(4 downto 3);
begin
  yt : entity work.oijxnajm
    port map (f => ishphaef, uqplel => eumvjxtqho, niao => pbwwkb);
  zjpqk : entity work.oijxnajm
    port map (f => yl, uqplel => thcolk, niao => bjexcejsaf);
  exfbh : entity work.oijxnajm
    port map (f => avkmbt, uqplel => ysuuapb, niao => wxpa);
  
  -- Multi-driven assignments
  oq <= "H";
  oq <= (others => '-');
end ihk;



-- Seed after: 6842825700551052274,14108379317686958507
