-- Seed: 12857087870074122874,3687118713772291287

entity hqv is
  port (yuiq : in time);
end hqv;

architecture xzerhjqa of hqv is
  
begin
  
end xzerhjqa;

entity opgilodcd is
  port (jbbmh : buffer time; r : inout time; a : in time; wiazqmjc : buffer time);
end opgilodcd;

architecture tynz of opgilodcd is
  signal ryc : time;
  signal hfslyu : time;
begin
  gxwb : entity work.hqv
    port map (yuiq => wiazqmjc);
  wxxmh : entity work.hqv
    port map (yuiq => hfslyu);
  cxuyxsdk : entity work.hqv
    port map (yuiq => wiazqmjc);
  nhgndgefaa : entity work.hqv
    port map (yuiq => ryc);
  
  -- Single-driven assignments
  jbbmh <= 8#6_3.4_1_1# ps;
  hfslyu <= 8#73.30122# fs;
  ryc <= 0 sec;
  r <= 1 hr;
end tynz;

library ieee;
use ieee.std_logic_1164.all;

entity yokitsgqba is
  port (xhfir : in integer_vector(2 downto 1); epfpub : buffer time; lfto : buffer std_logic_vector(1 downto 3); dk : out time);
end yokitsgqba;

architecture ywjmawbb of yokitsgqba is
  signal wetavjkj : time;
  signal nnfhyjrw : time;
begin
  wicndad : entity work.hqv
    port map (yuiq => dk);
  ekxoitxpnx : entity work.hqv
    port map (yuiq => nnfhyjrw);
  flefqxfhw : entity work.hqv
    port map (yuiq => wetavjkj);
  
  -- Single-driven assignments
  dk <= 2#1.0# us;
  
  -- Multi-driven assignments
  lfto <= (others => '0');
  lfto <= (others => '0');
  lfto <= "";
  lfto <= (others => '0');
end ywjmawbb;

library ieee;
use ieee.std_logic_1164.all;

entity gpmtd is
  port (trs : linkage std_logic_vector(2 to 3));
end gpmtd;

architecture srp of gpmtd is
  signal roj : time;
  signal qkypk : time;
  signal yejsutfva : time;
begin
  ntmenuiii : entity work.hqv
    port map (yuiq => yejsutfva);
  zrkyv : entity work.opgilodcd
    port map (jbbmh => qkypk, r => yejsutfva, a => yejsutfva, wiazqmjc => roj);
end srp;



-- Seed after: 7449148350754414398,3687118713772291287
