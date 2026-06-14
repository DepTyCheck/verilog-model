-- Seed: 12598583354966118909,6298042963991371649



entity whu is
  port (gkuzhnmkej : inout time_vector(2 downto 0));
end whu;



architecture h of whu is
  
begin
  
end h;



entity bj is
  port (hennhoa : out boolean; orqfblch : out time; lalwnl : linkage time_vector(2 downto 0));
end bj;



architecture lqev of bj is
  signal x : time_vector(2 downto 0);
  signal lkatnax : time_vector(2 downto 0);
  signal le : time_vector(2 downto 0);
  signal fdvq : time_vector(2 downto 0);
begin
  wukphuyqj : entity work.whu
    port map (gkuzhnmkej => fdvq);
  vlzfdrsbo : entity work.whu
    port map (gkuzhnmkej => le);
  t : entity work.whu
    port map (gkuzhnmkej => lkatnax);
  za : entity work.whu
    port map (gkuzhnmkej => x);
end lqev;

library ieee;
use ieee.std_logic_1164.all;

entity jxeezev is
  port (ylgvxdfu : inout integer; rvrtsfuev : buffer real; ndesoafx : inout integer_vector(0 downto 3); hrkhgqt : in std_logic_vector(2 downto 1));
end jxeezev;



architecture xrnqharghc of jxeezev is
  signal fqin : time_vector(2 downto 0);
  signal idyhnd : time_vector(2 downto 0);
begin
  e : entity work.whu
    port map (gkuzhnmkej => idyhnd);
  ywco : entity work.whu
    port map (gkuzhnmkej => fqin);
end xrnqharghc;



-- Seed after: 7614463740513760477,6298042963991371649
