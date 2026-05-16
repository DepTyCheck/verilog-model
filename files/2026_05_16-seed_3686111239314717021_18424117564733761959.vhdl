-- Seed: 3686111239314717021,18424117564733761959



entity vzkymtbcrt is
  port (fif : in real; px : in time);
end vzkymtbcrt;



architecture ipuixvony of vzkymtbcrt is
  
begin
  
end ipuixvony;



entity lbtyktoby is
  port (zfcoxlsm : linkage time);
end lbtyktoby;



architecture um of lbtyktoby is
  signal vvpzhm : time;
  signal za : real;
begin
  ol : entity work.vzkymtbcrt
    port map (fif => za, px => vvpzhm);
end um;



entity wdfuvw is
  port (st : inout bit);
end wdfuvw;



architecture womog of wdfuvw is
  signal dvptrpm : time;
  signal aeykshu : real;
begin
  chqjvqr : entity work.vzkymtbcrt
    port map (fif => aeykshu, px => dvptrpm);
  yugxj : entity work.lbtyktoby
    port map (zfcoxlsm => dvptrpm);
  uwhuconxko : entity work.vzkymtbcrt
    port map (fif => aeykshu, px => dvptrpm);
  dwj : entity work.vzkymtbcrt
    port map (fif => aeykshu, px => dvptrpm);
end womog;

library ieee;
use ieee.std_logic_1164.all;

entity dydzznq is
  port (saplmsqzl : out std_logic);
end dydzznq;



architecture fttewb of dydzznq is
  signal mdqvd : bit;
  signal i : bit;
  signal webglyzqjd : time;
  signal ihqivrcurl : real;
  signal rwndwy : time;
  signal bmvrsuj : real;
begin
  nrpwoz : entity work.vzkymtbcrt
    port map (fif => bmvrsuj, px => rwndwy);
  m : entity work.vzkymtbcrt
    port map (fif => ihqivrcurl, px => webglyzqjd);
  obs : entity work.wdfuvw
    port map (st => i);
  nivjgijeu : entity work.wdfuvw
    port map (st => mdqvd);
end fttewb;



-- Seed after: 17243251165388991538,18424117564733761959
