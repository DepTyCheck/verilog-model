-- Seed: 11527631294020720424,12143220691580258643

entity jz is
  port (atvfjwahg : buffer boolean_vector(2 downto 3); bxchjenwz : buffer time);
end jz;

architecture ml of jz is
  
begin
  -- Single-driven assignments
  bxchjenwz <= 2_3_3 ms;
  atvfjwahg <= (others => TRUE);
end ml;

library ieee;
use ieee.std_logic_1164.all;

entity ktutj is
  port (yjwrslh : inout time_vector(2 to 2); wh : buffer std_logic);
end ktutj;

architecture knr of ktutj is
  signal pile : time;
  signal tjftlds : boolean_vector(2 downto 3);
  signal snkn : time;
  signal lrk : boolean_vector(2 downto 3);
begin
  q : entity work.jz
    port map (atvfjwahg => lrk, bxchjenwz => snkn);
  rbezx : entity work.jz
    port map (atvfjwahg => tjftlds, bxchjenwz => pile);
  
  -- Single-driven assignments
  yjwrslh <= yjwrslh;
  
  -- Multi-driven assignments
  wh <= '0';
  wh <= 'Z';
end knr;

entity fmhh is
  port (vuqs : out time);
end fmhh;

architecture hwx of fmhh is
  signal ctzm : time;
  signal dte : boolean_vector(2 downto 3);
  signal s : boolean_vector(2 downto 3);
begin
  afv : entity work.jz
    port map (atvfjwahg => s, bxchjenwz => vuqs);
  eycg : entity work.jz
    port map (atvfjwahg => dte, bxchjenwz => ctzm);
end hwx;



-- Seed after: 637183367174812231,12143220691580258643
