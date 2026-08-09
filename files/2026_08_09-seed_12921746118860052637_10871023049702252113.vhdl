-- Seed: 12921746118860052637,10871023049702252113

entity slrqhdtmnp is
  port (exssme : in time);
end slrqhdtmnp;

architecture o of slrqhdtmnp is
  
begin
  
end o;

entity vdbixkf is
  port (aawcang : linkage boolean; waquxxdm : in string(3 downto 5); gnfnxgrbp : inout bit_vector(0 to 1); hkglmnxhz : in integer);
end vdbixkf;

architecture akrw of vdbixkf is
  
begin
  -- Single-driven assignments
  gnfnxgrbp <= ('1', '0');
end akrw;

library ieee;
use ieee.std_logic_1164.all;

entity mhmbsk is
  port (gzpvu : linkage std_logic_vector(3 to 0); hvo : buffer integer);
end mhmbsk;

architecture puf of mhmbsk is
  signal qwmutbqsg : time;
  signal vblgkyth : time;
  signal rqkkr : time;
  signal epd : integer;
  signal uqr : bit_vector(0 to 1);
  signal ouv : string(3 downto 5);
  signal qy : boolean;
begin
  qez : entity work.vdbixkf
    port map (aawcang => qy, waquxxdm => ouv, gnfnxgrbp => uqr, hkglmnxhz => epd);
  qmnx : entity work.slrqhdtmnp
    port map (exssme => rqkkr);
  yfngq : entity work.slrqhdtmnp
    port map (exssme => vblgkyth);
  gbqwuowjv : entity work.slrqhdtmnp
    port map (exssme => qwmutbqsg);
  
  -- Single-driven assignments
  hvo <= 2#1000#;
  rqkkr <= 4.213 ps;
  qwmutbqsg <= 0 hr;
  ouv <= "";
  epd <= hvo;
end puf;



-- Seed after: 9291144296164302656,10871023049702252113
