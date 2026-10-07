-- Seed: 14116016610758380915,5906004015519833893

library ieee;
use ieee.std_logic_1164.all;

entity xbg is
  port (tlzbolwdsu : buffer std_logic_vector(1 downto 1); iqbjocbv : out bit; hua : buffer character);
end xbg;

architecture krtpae of xbg is
  
begin
  -- Single-driven assignments
  hua <= hua;
  iqbjocbv <= '0';
  
  -- Multi-driven assignments
  tlzbolwdsu <= "W";
  tlzbolwdsu <= tlzbolwdsu;
end krtpae;

library ieee;
use ieee.std_logic_1164.all;

entity zhsatki is
  port (wqyoguxprs : buffer character; jrcgno : inout std_logic; nnslokhzhs : in std_logic; nayit : buffer integer);
end zhsatki;

library ieee;
use ieee.std_logic_1164.all;

architecture ys of zhsatki is
  signal ymziwmu : character;
  signal xezb : bit;
  signal o : std_logic_vector(1 downto 1);
  signal yyvkiq : bit;
  signal badacjwvlw : character;
  signal fmzonubf : bit;
  signal jytaihtkh : std_logic_vector(1 downto 1);
begin
  ywjgoyoyn : entity work.xbg
    port map (tlzbolwdsu => jytaihtkh, iqbjocbv => fmzonubf, hua => badacjwvlw);
  tmedsty : entity work.xbg
    port map (tlzbolwdsu => jytaihtkh, iqbjocbv => yyvkiq, hua => wqyoguxprs);
  n : entity work.xbg
    port map (tlzbolwdsu => o, iqbjocbv => xezb, hua => ymziwmu);
  
  -- Multi-driven assignments
  jrcgno <= nnslokhzhs;
end ys;



-- Seed after: 4420283241663468863,5906004015519833893
