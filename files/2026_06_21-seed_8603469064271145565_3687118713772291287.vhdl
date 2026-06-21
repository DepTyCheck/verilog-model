-- Seed: 8603469064271145565,3687118713772291287

entity fnctrrk is
  port (lnerjlhyz : inout time; jzyc : buffer bit_vector(0 to 4); mhvyp : inout character);
end fnctrrk;

architecture sqhu of fnctrrk is
  
begin
  -- Single-driven assignments
  mhvyp <= 'v';
  lnerjlhyz <= 1 sec;
  jzyc <= ('1', '0', '1', '0', '0');
end sqhu;

library ieee;
use ieee.std_logic_1164.all;

entity ek is
  port (lxdcma : linkage time; evoyqbdbaz : linkage std_logic; zwycvagozf : buffer std_logic_vector(1 to 1));
end ek;

architecture aemoc of ek is
  signal lljxrcvgtb : character;
  signal yhzgmmq : bit_vector(0 to 4);
  signal vucggbvwcw : time;
  signal cvwrxsr : character;
  signal qk : bit_vector(0 to 4);
  signal jttcxnjm : time;
  signal ipbpn : character;
  signal v : bit_vector(0 to 4);
  signal fnuxgs : time;
begin
  ubc : entity work.fnctrrk
    port map (lnerjlhyz => fnuxgs, jzyc => v, mhvyp => ipbpn);
  iuxnkge : entity work.fnctrrk
    port map (lnerjlhyz => jttcxnjm, jzyc => qk, mhvyp => cvwrxsr);
  avnrocp : entity work.fnctrrk
    port map (lnerjlhyz => vucggbvwcw, jzyc => yhzgmmq, mhvyp => lljxrcvgtb);
  
  -- Multi-driven assignments
  zwycvagozf <= (others => 'L');
end aemoc;



-- Seed after: 15944310309277896905,3687118713772291287
